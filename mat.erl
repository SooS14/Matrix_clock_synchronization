-module(mat).
-export([start/2, start_process/4, test/1]).


% Call this function to start the program.
start(N, Test_function) ->
    io:format("Création d'un full mesh de taille ~w~n", [N]),
    M = N,
    start_process(N, M, [], Test_function).


% Start the processes, N is an identifier, M is the total number of processes
% The PIDs are stored in a list for the test function.
start_process(1, M, Pid_list, Test_function) -> 
    Pid = spawn(node, do_something, [1, M]),
    Pid_list_2 = [Pid | Pid_list],
    node:print_pid_list(Pid_list_2),

    case Test_function of
        test_random ->
            {ok,Event} = io:read("do you want random event unrelated to messages ? y/n : "),
            {ok,Delay} = io:read("do you want the messages to use random delay ? y/n : "),
            {ok,Sleep} = io:read("do you want the supervisor program to ocasionaly sleep during a random time ? y/n : "),
            {ok,Matrix} = io:read("do you want to print the matrix at each step ? y/n : "),
            {ok,Number} = io:read("state the number of message to be sent <int> : "),
            test_random(Pid_list_2, Event, Delay, Sleep, Matrix, Number);
        exercise_5 ->
            exercise_5(Pid_list_2);
        true ->
            test(Pid_list_2)
    end;
    
    
start_process(N, M, Pid_list, Test_function) ->
    Pid = spawn(node, do_something, [N, M]),
    start_process(N-1, M, [Pid | Pid_list], Test_function).



% Event -> Boolean, do you want random event unrelated to messages ?
% Delay -> Boolean, do you want the messages to use random delay ?
% Sleep -> Boolean, do you want the supervisor program to ocasionaly 
%           sleep during a random time ?
% Matrix -> Boolean, do you want to print the matrix at each step ?
% Msg_per_proc -> Int, state the number of message to be sent.
%
% See markdown file for details
test_random(Pid_list, _, _, _,_, 0) ->
    timer:sleep(1000),
    L = length(Pid_list),
    give_matrix(L, Pid_list),
    exit(correct_end_of_the_program);

test_random(Pid_list, Event, Delay, Sleep, Matrix, Number) ->
    L = length(Pid_list),
    Sender = rand:uniform(L),
    Receiver = rand:uniform(L),
    if
        Event == y -> 
            Test = rand:uniform(3),
            if 
                Test == 3 -> 
                    lists:nth(Sender,Pid_list) ! {order_event},
                    io:format("event on ~w ~n", [Test]);
                true -> ok
            end;
        Event == n -> ok
    end,
    if
        Sleep == y ->
            Test3 = rand:uniform(3),
            if
                Test3 == 3 ->
                    Test1 = rand:uniform(10) * 50,
                    io:format("sleep : ~w ~n", [Test1]),
                    timer:sleep(Test1);
                true -> ok
            end;
        Sleep == n -> ok
    end,
    if
        Delay == y -> 
            Test2 = rand:uniform(10) * 50,
            lists:nth(Sender,Pid_list) ! {order_send_msg_delay,Test2,lists:nth(Receiver,Pid_list), Receiver},
            io:format("send msg : ~w -> ~w, delay : ~w ~n", [Sender, Receiver, Test2]);
        Delay == n ->
            lists:nth(Sender,Pid_list) ! {order_send_msg,lists:nth(Receiver,Pid_list), Receiver},
            io:format("send msg : ~w -> ~w ~n", [Sender, Receiver])
    end,
    if
        Matrix == y -> 
            give_matrix(L, Pid_list);                
        Matrix == n -> ok
    end,
    test_random(Pid_list, Event, Delay, Sleep, Matrix, Number-1).


give_matrix(1, Pid_list) -> 
    lists:nth(1,Pid_list) ! {give_id_and_matrix};

give_matrix(L, Pid_list) ->
    lists:nth(L,Pid_list) ! {give_id_and_matrix},
    give_matrix(L-1, Pid_list).



% Use this function to test the program, just state your orders.
% To see what are the orders you can give to the processes, see the receive 
% block in the function "node:do_something(N, M, My_mat, Store_stamp)".
% You can also read the markdown file. Or even use the mat:random_test 
% function, which generate random orders.
%
% The code below is just an example, be sure to create enough processes.
test(Pid_list) ->
    lists:nth(1,Pid_list) ! {order_send_msg_delay, 125, lists:nth(2,Pid_list), 2},
    lists:nth(1,Pid_list) ! {order_send_msg, lists:nth(2,Pid_list), 2},
    lists:nth(1,Pid_list) ! {give_id_and_matrix},
    lists:nth(2,Pid_list) ! {give_id_and_matrix},

    timer:sleep(150),

    lists:nth(1,Pid_list) ! {order_send_msg_delay, 125, lists:nth(2,Pid_list), 2},
    lists:nth(2,Pid_list) ! {order_event},
    lists:nth(2,Pid_list) ! {order_send_msg, lists:nth(2,Pid_list), 2},

    timer:sleep(250),
    lists:nth(1,Pid_list) ! {give_id_and_matrix},
    lists:nth(2,Pid_list) ! {give_id_and_matrix},
    exit(correct_end_of_the_program).


% To show the correct execution of the program.
% The events are those from the fith exercise 
% of the first exercise sheet. For more, see 
% the file "td1-tps-logique-correction.pdf"
exercise_5(Pid_list) ->
    lists:nth(1,Pid_list) ! {order_send_msg_delay,125,lists:nth(2,Pid_list), 2},
    lists:nth(2,Pid_list) ! {order_send_msg, lists:nth(1,Pid_list), 1},
    lists:nth(3,Pid_list) ! {order_send_msg, lists:nth(2,Pid_list), 2},

    timer:sleep(62),
    lists:nth(2,Pid_list) ! {order_send_msg, lists:nth(3,Pid_list), 3}, 

    timer:sleep(250),

    lists:nth(1,Pid_list) ! {order_send_msg_delay,250,lists:nth(3,Pid_list), 3},
    lists:nth(1,Pid_list) ! {order_send_msg_delay,62,lists:nth(2,Pid_list), 2},
    lists:nth(2,Pid_list) ! {order_send_msg_delay, 62, lists:nth(3,Pid_list), 3},    
    lists:nth(3,Pid_list) ! {order_send_msg, lists:nth(1,Pid_list), 1},
    timer:sleep(125),    
    lists:nth(3,Pid_list) ! {order_send_msg, lists:nth(2,Pid_list), 2},

    timer:sleep(250),

    lists:nth(2,Pid_list) ! {order_send_msg_delay, 125, lists:nth(3,Pid_list), 3},    
    lists:nth(1,Pid_list) ! {order_send_msg,lists:nth(3,Pid_list), 3},
    lists:nth(2,Pid_list) ! {order_send_msg, lists:nth(1,Pid_list), 1},
    timer:sleep(62),
    lists:nth(1,Pid_list) ! {order_send_msg,lists:nth(3,Pid_list), 3},
    lists:nth(1,Pid_list) ! {order_send_msg,lists:nth(2,Pid_list), 2},
    timer:sleep(125),
    lists:nth(3,Pid_list) ! {order_send_msg, lists:nth(2,Pid_list), 2},

    timer:sleep(250),

    lists:nth(1,Pid_list) ! {give_id_and_matrix},
    lists:nth(2,Pid_list) ! {give_id_and_matrix},
    lists:nth(3,Pid_list) ! {give_id_and_matrix},
    exit(correct_end_of_the_program).

