#include <alloca.h>
#include <alsa/asoundlib.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

static snd_seq_t *g_seq = NULL;
static int g_port;
static snd_midi_event_t *g_event;
static int g_conn_client = -1, g_conn_port;

#define INPUT_CAP  (SND_SEQ_PORT_CAP_READ  | SND_SEQ_PORT_CAP_SUBS_READ)
#define OUTPUT_CAP (SND_SEQ_PORT_CAP_WRITE | SND_SEQ_PORT_CAP_SUBS_WRITE)

enum {
	INPUT_DEVICE,
	OUTPUT_DEVICE
};

CAMLprim value
io_init()
{
	CAMLparam0();
	if (snd_seq_open(&g_seq, "hw", SND_SEQ_OPEN_DUPLEX, 0) < 0)
		caml_failwith("snd_seq_open() failed");

	g_port = snd_seq_create_simple_port(
		g_seq,
		"main",
		INPUT_CAP,
		SND_SEQ_PORT_TYPE_MIDI_GENERIC
	);
	if (g_port < 0)
		caml_failwith("snd_seq_create_simple_port() failed");
	if (snd_midi_event_new(1024, &g_event) < 0)
		caml_failwith("snd_midi_event_new() failed");

	CAMLreturn(Val_unit);
}

CAMLprim value
io_fini()
{
	CAMLparam0();
	snd_seq_close(g_seq);
	snd_midi_event_free(g_event);
	CAMLreturn(Val_unit);
}

CAMLprim value
io_enum_devices(value type)
{
	CAMLparam1(type);
	CAMLlocal2(ret, cons);

	if (Long_val(type) != OUTPUT_DEVICE)
		caml_failwith("unsupported device type");

	if (g_seq == NULL)
		caml_failwith("you should io_init() first");

	snd_seq_client_info_t *cinfo;
	snd_seq_port_info_t *pinfo;
	int client;

	snd_seq_client_info_alloca(&cinfo);
	snd_seq_client_info_set_client(cinfo, -1);

	ret = Val_emptylist;
	while (snd_seq_query_next_client(g_seq, cinfo) >= 0) {
		client = snd_seq_client_info_get_client(cinfo);
		snd_seq_port_info_alloca(&pinfo);
		snd_seq_port_info_set_client(pinfo, client);

		snd_seq_port_info_set_port(pinfo, -1);
		while (snd_seq_query_next_port(g_seq, pinfo) >= 0) {
			if ((snd_seq_port_info_get_type(pinfo) & SND_SEQ_PORT_TYPE_MIDI_GENERIC) == 0)
				continue;
			if ((snd_seq_port_info_get_capability(pinfo) & OUTPUT_CAP) != OUTPUT_CAP)
				continue;

			char id[64];
			sprintf(
				id,
				"alsa/%i:%i",
				snd_seq_port_info_get_client(pinfo),
				snd_seq_port_info_get_port(pinfo)
			);

			cons = caml_alloc(2, 0);
			Store_field(cons, 0, caml_copy_string(id));
			Store_field(cons, 1, ret);
			ret = cons;
		}
	}

	CAMLreturn(ret);
}

CAMLprim value
io_set_device(value type, value id)
{
	CAMLparam2(type, id);
	if (Long_val(type) != OUTPUT_DEVICE)
		caml_failwith("unsupported device type");

	int client, port;
	if (sscanf(String_val(id), "alsa/%i:%i", &client, &port) < 2)
		caml_failwith("can't parse sequencer port; should be alsa/<client id>:<port id>");

	if (snd_seq_connect_to(g_seq, g_port, client, port) < 0)
		caml_failwith("snd_seq_connect_to() failed");

	if (g_conn_client >= 0)
		snd_seq_disconnect_to(g_seq, g_port, g_conn_client, g_conn_port);
	g_conn_client = client;
	g_conn_port = port;
	CAMLreturn(Val_unit);
}

CAMLprim value
io_output(value raw)
{
	CAMLparam1(raw);

	snd_seq_event_t ev;
	snd_seq_ev_clear(&ev);
	if (snd_midi_event_encode(g_event, String_val(raw), caml_string_length(raw), &ev) <= 0)
		caml_failwith("snd_midi_event_encode() failed");

	if (ev.type == SND_SEQ_EVENT_NONE)
		caml_failwith("ev.type == SND_SEQ_EVENT_NONE");

	snd_seq_ev_set_source(&ev, g_port);
	snd_seq_ev_set_subs(&ev);
	snd_seq_ev_set_direct(&ev);

	if (snd_seq_event_output(g_seq, &ev) < 0)
		caml_failwith("snd_seq_event_output() failed");
	CAMLreturn(Val_unit);
}

CAMLprim value
io_flush_output()
{
	CAMLparam0();
	snd_seq_drain_output(g_seq);
	CAMLreturn(Val_unit);
}
