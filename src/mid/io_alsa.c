#include <alsa/asoundlib.h>
#include <caml/mlvalues.h>

static snd_seq_t *seq;

CAMLprim value
io_init(value v)
{
	if (snd_seq_open(&seq, "hw", SND_SEQ_OPEN_DUPLEX, 0) < 0)
		return Val_bool(0);

	return Val_bool(1);
}
