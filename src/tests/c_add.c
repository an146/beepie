#include <caml/mlvalues.h>

CAMLprim value c_add(value a, value b)
{
	return Val_long(Long_val(a) + Long_val(b));
}
