#ifndef __GUILE_HELPERS_H__
#define __GUILE_HELPERS_H__

#include <libguile.h>

void scm_add_to_load_path(const char *path);

#define SCM_CALL(module, fn) (scm_call_0(scm_c_public_ref(module, fn)))
#define SCM_CALL_(nr, module, fn, ...) (scm_call_ ## nr (scm_c_public_ref(module, fn), __VA_ARGS__))

#define SCM_LOAD(file) (scm_primitive_load_path(scm_from_locale_string(file)))

#endif // __GUILE_HELPERS_H__
