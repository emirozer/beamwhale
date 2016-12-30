/* posix.c */
#include "erl_nif.h"
#include <stdlib.h>
#include <pwd.h>
#include <stdio.h>
#include <string.h>

#define ROOT "root"

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


static ERL_NIF_TERM get_user(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    if (strcmp(getUserName(), ROOT) == 0)
    {
        return enif_make_string(env, ROOT,ERL_NIF_LATIN1);
    } else {
        return enif_make_string(env, getUserName(),ERL_NIF_LATIN1);
    }
    
}

static ErlNifFunc nif_funcs[] =
{
    {"get_user", 0, get_user}
};

ERL_NIF_INIT(posix,nif_funcs,NULL,NULL,NULL,NULL)


