
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

CAMLprim value tiny_httpd_ws_apply_masking(value _mask_key, value _buf,
                                           value _offset, value _len) {
  CAMLparam4(_mask_key, _buf, _offset, _len);

  char const *mask_key = String_val(_mask_key);
  char *buf = Bytes_val(_buf);
  intnat offset = Int_val(_offset);
  intnat len = Int_val(_len);

  for (intnat i = 0; i < len; ++i) {
    unsigned char c = buf[offset + i];
    unsigned char c_m = mask_key[i & 0x3];
    buf[offset + i] = (unsigned char)(c ^ c_m);
  }
  CAMLreturn(Val_unit);
}
