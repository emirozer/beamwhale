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

static ERL_NIF_TERM get_group_id(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return enif_make_int(env, getegid());
}

static ERL_NIF_TERM syscall_libc(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int syscall_code;
    int flags;
    enif_get_int(env, argv[0], &syscall_code);
    enif_get_int(env, argv[1], &flags);
    int r = syscall(syscall_code, flags);
    return enif_make_int(env, r);
}

static ERL_NIF_TERM mount_libc(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int r = 0;
    
    char source[MAXBUFLEN];
    char target[MAXBUFLEN];
    char fs[MAXBUFLEN];
    int flags;
    char options[MAXBUFLEN];
    
    int r_egs_source = enif_get_string(env, argv[0], source, sizeof(source), ERL_NIF_LATIN1);
    int r_egs_target = enif_get_string(env, argv[1], target, sizeof(target), ERL_NIF_LATIN1);
    int r_egs_fs = enif_get_string(env, argv[2], fs, sizeof(fs), ERL_NIF_LATIN1);
    enif_get_int(env, argv[3], &flags);
    int r_egs_options = enif_get_string(env, argv[4], options, sizeof(options), ERL_NIF_LATIN1);

    if (r_egs_source < 1 || r_egs_target < 1 || r_egs_fs < 1 || r_egs_options < 1 )
    {
        return enif_make_badarg(env);
    }
    
    if ((strcmp(options, "NULL") == 0) && (strcmp(fs, "NULL") == 0))
    {
        r = mount(source, target, NULL, flags, NULL);
    } else if (strcmp(fs, "NULL") == 0)
    {
        r = mount(source, target, NULL, flags, options);
    } else if (strcmp(options, "NULL") == 0)
    {
        r = mount(source, target, fs, flags, NULL);
    } else
    {
        r = mount(source, target, fs, flags, options);
    }
    
    if (r != 0)
    {
        printf("mount failed: %d", r);
    }    
    
    return enif_make_int(env, r);
    
}


static ERL_NIF_TERM umount_libc(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    char mountpoint[MAXBUFLEN];
    if (enif_get_string(env, argv[0], mountpoint, MAXBUFLEN, ERL_NIF_LATIN1) < 1)
    {
        return enif_make_badarg(env);
    }
    
    int code = umount(mountpoint);
    return enif_make_int(env, code);
}

static ERL_NIF_TERM fork_libc(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int pid = fork();
    return enif_make_int(env, pid);
}


static ERL_NIF_TERM waitpid_libc(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int current;
    int expected;
    enif_get_int(env, argv[0], &current);
    enif_get_int(env, argv[1], &expected);
    waitpid(current, expected, 0);
}


static ERL_NIF_TERM exit_libc(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int code;
    enif_get_int(env, argv[0], &code);
    exit(code);
}

static ERL_NIF_TERM unshare_libc(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int flags;
    enif_get_int(env, argv[0], &flags);
    int code = unshare(flags);
    return enif_make_int(env, code);
}


static ERL_NIF_TERM get_pid(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return enif_make_int(env, getpid());
}

static ERL_NIF_TERM pivot(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    char new_root[MAXBUFLEN];
    char old_root[MAXBUFLEN];
    
    int r_egs_newroot = enif_get_string(env, argv[0], new_root, sizeof(new_root), ERL_NIF_LATIN1);
    int r_egs_oldroot = enif_get_string(env, argv[1], old_root, sizeof(old_root), ERL_NIF_LATIN1);

    if ( r_egs_newroot < 1 || r_egs_oldroot < 1 )
    {
        return enif_make_badarg(env);
    }
    
    int code = pivot_root(new_root, old_root);
    return enif_make_int(env, code);
}

static ERL_NIF_TERM exec_libc(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    char command[MAXBUFLEN];
    char args[MAXBUFLEN];
    
    int r_egs_command = enif_get_string(env, argv[0], command, sizeof(command), ERL_NIF_LATIN1);
    int r_egs_args = enif_get_string(env, argv[1], args, sizeof(args), ERL_NIF_LATIN1);

    if ( r_egs_command < 1 || r_egs_args < 1 )
    {
        return enif_make_badarg(env);
    }

    int code = execl(command, command, args);
    return enif_make_int(env, code);
}


static ERL_NIF_TERM set_hostname(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    char hostname[MAXBUFLEN];
    if (enif_get_string(env, argv[0], hostname, MAXBUFLEN, ERL_NIF_LATIN1) < 1)
    {
        return enif_make_badarg(env);
    }
    int code = sethostname(hostname);
    return enif_make_int(env, code);
}


static ErlNifFunc nif_funcs[] =
{
    {"get_user", 0, get_user},
    {"get_group_id", 0, get_group_id},
    {"mount_libc", 5, mount_libc},
    {"umount_libc", 1, umount_libc},
    {"fork_libc", 0, fork_libc},
    {"waitpid_libc", 2, waitpid_libc},
    {"exit_libc", 1, exit_libc},
    {"unshare_libc", 1, unshare_libc},
    {"syscall_libc", 2, syscall_libc},
    {"get_pid", 0, get_pid},
    {"pivot", 2, pivot},
    {"exec_libc", 2, exec_libc},
    {"set_hostname", 1, set_hostname}
};

ERL_NIF_INIT(posix,nif_funcs,NULL,NULL,NULL,NULL)


