-module(directory).
-export([del_dir/1, traverse/1]).

del_dir(Dir) ->
    lists:foreach(fun(D) ->
                          ok = file:del_dir(D)
                  end, del_all_files([Dir], [])).

del_all_files([], EmptyDirs) ->
    EmptyDirs;
del_all_files([Dir | T], EmptyDirs) ->
    {ok, FilesInDir} = file:list_dir(Dir),
    {Files, Dirs} = lists:foldl(fun(F, {Fs, Ds}) ->
                                        Path = Dir ++ "/" ++ F,
                                        case filelib:is_dir(Path) of
                                            true ->
                                                {Fs, [Path | Ds]};
                                            false ->
                                                {[Path | Fs], Ds}
                                        end
                                end, {[],[]}, FilesInDir),
    lists:foreach(fun(F) ->
                          ok = file:delete(F)
                  end, Files),
    del_all_files(T ++ Dirs, [Dir | EmptyDirs]).

traverse(Dir) ->
    {ok, Cwd} = file:get_cwd(),
    case file:set_cwd(Dir) of
        ok ->
            {ok, Filenames} = file:list_dir("."),
            F = fun(Name) ->
                        case filelib:is_dir(Name) of
                            true  -> traverse(Name);
                            false -> traverse_process(Name)
                        end
                end,
            lists:foreach(F, Filenames);
        {error, Reason} ->
            io:format("~p error reason : ~s~n", [Dir, Reason])
    end,
    file:set_cwd(Cwd).

traverse_process(Name) -> io:format("~s~n", [Name]).
