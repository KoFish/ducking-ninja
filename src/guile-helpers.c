#include "guile-helpers.h"

void scm_add_to_load_path(const char *path) {
    SCM load_hook = scm_c_lookup("%load-path");
    scm_variable_set_x(load_hook, scm_cons(scm_from_locale_string(path), scm_variable_ref(load_hook)));
}

