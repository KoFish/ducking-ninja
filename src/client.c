#include <stdio.h>
#include <libguile.h>

#include "guile-helpers.h"
#include "debug.h"

SCM game_start_cb(void *_screen) {
    SCM screen = (SCM)_screen;
    DBG("Start game\n");
    SCM_CALL_(1, "game main-menu", "run", screen);
    scm_throw(scm_from_locale_symbol("game-done"), scm_cons(scm_from_locale_string("We have reached the end, my friend"), SCM_EOL));
    return SCM_BOOL_F;
}

SCM run_with_screen(void *_screen) {
    SCM game_start_module = scm_c_resolve_module("game start");
    scm_c_call_with_current_module(game_start_module, game_start_cb, _screen);
    return SCM_BOOL_T;
}

SCM terminate_with_screen(void *data, SCM key, SCM args) {
    char *key_str;
    DBG("Setup screen\n");
    SCM_CALL("game screen-setup", "destroy");
    key_str = scm_to_locale_string(scm_symbol_to_string(key));
    if (strncmp(key_str, "game-done", 9) == 0) {
        char *msg = scm_to_locale_string(scm_car(args));
        DBG("Game finished successfully\n");
        printf("%s\n", msg);
        free(key_str);
        free(msg);
    } else {
        DBG("Game failed with key %s\n", key_str);
        if (key_str != NULL) free(key_str);
        scm_throw(key, args);
    }
    return SCM_BOOL_F;
}

void *guile_run(void *arg) {
    SCM screen;
    scm_add_to_load_path("./scm/");
    SCM_LOAD("initialize.scm");

    scm_c_use_module("game screen-setup");

    screen = SCM_CALL("game screen-setup", "setup");

    scm_internal_catch(SCM_BOOL_T, run_with_screen, (void *)screen, terminate_with_screen, NULL);

    return NULL;
}

int main(int argc, char *argv[]) {
    scm_with_guile(guile_run, NULL);
    return 0;
}
