-module(node).
-export([do_something/2, print_pid_list/1, print_pid_list/2]).


% This function is executed by each process when starting.
% It receives the messages sent by the supervisor and by other processes
do_something(N, M) ->
    My_mat = array:new([{size,M*M},{fixed,true},{default,0}]),
    Store_stamp = [],
    do_something(N, M, My_mat, Store_stamp).

do_something(N, M, My_mat, Store_stamp) ->
    receive
        % When receiving this message, the process give his id and his matrix.
        {give_id_and_matrix} ->
            print_matrix(N, M, My_mat),
            My_mat2 = My_mat,
            Store_stamp2 = Store_stamp;

        % When receiving an order, the process updates his matrix
        % and he sends the message with his number and the stamp.
        {order_send_msg, Pid_dest, Num_dest} ->
            My_mat2 = update_mat_send(N, Num_dest, M, My_mat),
            Store_stamp2 = Store_stamp,
            Pid_dest ! {std_msg, N, My_mat2};

        % When receiving this order, the process updates his matrix
        % and he sends a delayed message with his number and the stamp.
        % The delay time is given by the commanding process.
        {order_send_msg_delay, Time, Pid_dest, Num_dest} ->
            My_mat2 = update_mat_send(N, Num_dest, M, My_mat),
            Store_stamp2 = Store_stamp,
            erlang:send_after(Time, Pid_dest, {std_msg, N, My_mat2});

        % When receiving a message, the process checks if he can deliver
        % the message. If not, it is stored and the clock is not updated.
        {std_msg, N_src, Stamp} ->
            Cond = check_mat_recv(N_src, Stamp, N, My_mat, M),
            if
                Cond == 1 -> 
                    My_mat2 = update_mat_recv(N_src, Stamp, N, My_mat, M),
                    Store_stamp2 = Store_stamp;
                Cond == 0 -> 
                    Store_stamp2 = [[N_src, Stamp] | Store_stamp],
                    My_mat2 = My_mat
            end;

        % This order tells the process to consider a basic event during its execution.
        {order_event} ->            
            My_mat2 = update_mat_event(N, M, My_mat),
            Store_stamp2 = Store_stamp
    end,
    My_mat3 = process_store_stamp(Store_stamp2, N, My_mat2, M),
    do_something(N, M, My_mat3, Store_stamp2).



% This function parses the list Store_stamp, it contains
% the messages that couldn't be delivered since they
% didn't met the condition specified by the algorithm.
process_store_stamp([], _, Clock, _) -> Clock;

process_store_stamp(Store_stamp, N, Clock, M) ->
    [H|T] = Store_stamp,
    [N_src, Stamp] = H,
    Cond = check_mat_recv(N_src, Stamp, N, Clock, M),
    if 
        Cond == 1 ->
            Clock2 = update_mat_recv(N_src, Stamp, N, Clock, M),
            process_store_stamp(Store_stamp, N, Clock2, M);
        Cond == 0 ->
            process_store_stamp(T, N, Clock, M)
    end.


% Updates the matrix when a message is sent
update_mat_send(N, Num_dest, M, Mat) ->
    Pos = (N - 1) * M + N - 1,
    Integer = array:get(Pos, Mat),
    Mat2 = array:set(Pos, Integer + 1, Mat),
    Pos2 = (N - 1) * M + Num_dest - 1,
    Integer2 = array:get(Pos2, Mat2),
    array:set(Pos2, Integer2 + 1, Mat2).


% Checks the matrix when a message is received
check_mat_recv(N_src, Stamp, N, Clock, M) ->
    Pos = (N_src - 1) * M + N - 1,
    Stamp_val = array:get(Pos, Stamp),
    Clock_val = array:get(Pos, Clock),
    if
        Stamp_val /= Clock_val + 1 -> 0;
        Stamp_val == Clock_val + 1 ->
            check_rest_column(N_src, Stamp, N, Clock, M, M)
    end.


check_rest_column(_, _, _, _, _, 0) -> 1;

check_rest_column(N_src, Stamp, N, Clock, M, Count_line) ->
    if
        Count_line == N_src -> 
            check_rest_column(N_src, Stamp, N, Clock, M, Count_line-1);
        Count_line == N -> 
            check_rest_column(N_src, Stamp, N, Clock, M, Count_line-1);
        true -> 
            Pos = (Count_line - 1) * M + N - 1,
            Stamp_val = array:get(Pos, Stamp),
            Clock_val = array:get(Pos, Clock),
            if
                Stamp_val > Clock_val -> 0;
                Stamp_val =< Clock_val -> 
                    check_rest_column(N_src, Stamp, N, Clock, M, Count_line-1)
            end
    end.
    

% Updates the matrix when a message is received
update_mat_recv(N_src, Stamp, N, Clock, M) ->
    Pos = (N - 1) * M + N - 1,
    Clock_val = array:get(Pos, Clock),
    Clock2 = array:set(Pos, Clock_val + 1, Clock),

    Pos2 = (N_src - 1) * M + N - 1,
    Clock_val2 = array:get(Pos2, Clock2),
    Clock3 = array:set(Pos2, Clock_val2 + 1, Clock2),   
    
    update_rest_mat(N_src, Stamp, N, Clock3, M, M*M).


update_rest_mat(_, _, _, Clock, _, 0) -> Clock;

update_rest_mat(N_src, Stamp, N, Clock, M, Count) ->
    if
        Count == (N_src - 1) * M + N -> 
            update_rest_mat(N_src, Stamp, N, Clock, M, Count - 1);
        Count == (N - 1) * M + N -> 
            update_rest_mat(N_src, Stamp, N, Clock, M, Count - 1);
        true ->
            Clock_val = array:get(Count-1, Clock),
            Stamp_val = array:get(Count-1, Stamp),
            if
                Clock_val < Stamp_val ->
                    update_rest_mat(N_src, Stamp, N, array:set(Count-1, Stamp_val, Clock), M, Count - 1);
                Clock_val >= Stamp_val ->
                    update_rest_mat(N_src, Stamp, N, Clock, M, Count - 1)
            end
    end.


% Updates the matrix when an event occurs
update_mat_event(N, M, Mat) ->
    Pos = (N - 1) * M + N - 1,
    Integer = array:get(Pos, Mat),
    array:set(Pos, Integer + 1, Mat).





% Prints a given matrix, it needs the total number of processes.
print_matrix(N, M, Mat) ->
    Length = array:size(Mat) - 1,
    print_matrix(N, M, Mat, Length, "~n").

print_matrix(N, _, _, -1, Buff3) ->
    Buff = string:concat("I am ",integer_to_list(N)),
    Buff2 = string:concat(Buff, ", and this is my matrix : "),
    Buff4 = string:concat(Buff2, Buff3),
    io:format(Buff4);

print_matrix(N, M, Mat, Length, Buff) ->
    Integer = array:get(Length, Mat),
    Buff2 = integer_to_list(Integer),
    if
        Integer < 10 -> Buff3 = string:concat("      ", Buff2);
        Integer < 100 -> Buff3 = string:concat("     ", Buff2);
        Integer < 1000 -> Buff3 = string:concat("    ", Buff2);
        Integer < 10000 -> Buff3 = string:concat("   ", Buff2);
        Integer < 100000 -> Buff3 = string:concat("  ", Buff2);
        true -> Buff3 = string:concat(" ", Buff2)
    end,
    Buff4 = string:concat(Buff3, Buff),
    if
        Length rem M == 0 -> Buff5 = string:concat("~n", Buff4);
        true -> Buff5 = Buff4
    end,
    print_matrix(N, M, Mat, Length - 1, Buff5).


% Prints the list of Pid.
print_pid_list(Pid_list) ->
    N = length(Pid_list),
    io:format("list of pids :~n"),
    print_pid_list(Pid_list, N).

print_pid_list(Pid_list, 1) ->
    io:format("Node ~w ; pid = ~w ~n",[1, lists:nth(1, Pid_list)]),
    io:format("end list~n");

print_pid_list(Pid_list, N) ->
    io:format("Node ~w ; pid = ~w ~n",[N, lists:nth(N, Pid_list)]),
    print_pid_list(Pid_list, N-1).