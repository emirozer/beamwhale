/* posix.c */
#include "erl_nif.h"
#include <stdlib.h>
#include <pwd.h>
#include <stdio.h>

const char *getUserName()
{
    uid_t uid = geteuid();
    struct passwd *pw = getpwuid(uid);
    if (pw)
    {
        return pw->pw_name;
    }

    return "";
}


static ERL_NIF_TERM is_user_root(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    if (strcmp(getUserName(), "root") == 0)
    {
        return enif_make_int(env, 1);
    } else {
        return enif_make_int(env, 0);
    }
    
}

static ErlNifFunc nif_funcs[] =
{
    {"is_user_root", 0, is_user_root}
};

ERL_NIF_INIT(posix,nif_funcs,NULL,NULL,NULL,NULL)


