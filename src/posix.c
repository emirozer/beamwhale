/* posix.c */
#include "erl_nif.h"
#include <stdlib.h>
#include <pwd.h>
#include <stdio.h>
#include <string.h>
#include <sys/mount.h>

#define ROOT "root"
#define MAXBUFLEN  1024

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

static ERL_NIF_TERM mount_libc(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int r = 0;
    
    char source;
    char target;
    char fs;
    int flags;
    char options;
    enif_get_string(env, argv[0], &source, MAXBUFLEN, ERL_NIF_LATIN1);
    enif_get_string(env, argv[1], &target, MAXBUFLEN, ERL_NIF_LATIN1);
    enif_get_string(env, argv[2], &fs, MAXBUFLEN, ERL_NIF_LATIN1);
    enif_get_int(env, argv[3], &flags);
    enif_get_string(env, argv[4], &options, MAXBUFLEN, ERL_NIF_LATIN1);
    
    if ((strcmp(&options, "NULL") == 0) && (strcmp(&fs, "NULL") == 0)){
        r = mount(&source, &target, NULL, flags, NULL);
    } else if (strcmp(&fs, "NULL") == 0) {
        r = mount(&source, &target, NULL, flags, &options);
    }else if (strcmp(&options, "NULL") == 0) {
        r = mount(&source, &target, &fs, flags, NULL);
    } else {
        r = mount(&source, &target, &fs, flags, &options);
    }
    
    if (r != 0){
        printf("mount failed: %d", r);
    }    
    
    return enif_make_int(env, r);
    
}

static ERL_NIF_TERM fork_libc(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int pid = fork();
    return enif_make_int(env, pid);
}

static ErlNifFunc nif_funcs[] =
{
    {"get_user", 0, get_user},
    {"mount_libc", 5, mount_libc},
    {"fork_libc", 0, fork_libc}
};

ERL_NIF_INIT(posix,nif_funcs,NULL,NULL,NULL,NULL)


