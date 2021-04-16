
# Documentation Erlang project - Matrix stamping

___


### Module mat.erl

**start(N, Test_function)**
>Call this function to start the program.

+ int N : number of process to start.
+ atom Test_function : the name of the function to use to test the program
  - test_random (see **test_random**)
  - exercise_5 (see file : td1-tps-logique-correction.pdf)
  - test (if you want to design your own test, put your code here)


**start_process(N, M, Pid_list, Test_function)**
>Starts N processes. Newly created processes use the **do_something** function.

+ int N : an identifier for each process.
+ int M : the total number of processes.
+ list Pid_list : stores the pids of the processes for the test function.
+ atom Test_function : function to test the program, if test_random is given, it asks a few details needed for the test.


**test_random(Pid_list, Event, Delay, Sleep, Matrix, Number)**
>This function can be used to test the program by sending random orders to 
the processes. To use it, just launch the program with the **start** function
and give the atomic value "test_random" as a second argument.
When used, the program will ask you a few information before running the program,
see below :

+ bool Event : do you want to generate random events unrelated to messages ?
  - y or n ; if y, processes have 1/3 chance to generate local events.
+ bool Delay : do you want the messages to be delayed with a random time ?
  - y or n ; if y, a delay (1-500 ms) is applied to all messages.
+ bool Sleep : do you want the supervisor program to ocasionaly sleep during a random time ?
  - y or n ; if y, processes have 1/3 chance to sleep during 1-500 ms.
+ bool Matrix : do you want to print the matrix at each step ?
 - y or n ; if y, prints all the matrixes at each step.
+ int Msg_per_proc : state the number of message to be sent.

>At the end of the test, the program sleep during a second and then prints the 
clock matrix of each process. 



**give_matrix(L, Pid_list)**
>Orders each running process to print its matrix on the standard output.

int L : identifier of the process.
list Pid_list : contains the PID of each runnig process. 


**test(Pid_list)**
>Use this function to test the program, just state your orders.
list Pid_list : is the list contain the PIDs of all running processes.
Here are the available orders you can give to the processes :

lists:nth(X,Pid_list) ! {order_send_msg_delay, Y, lists:nth(Z,Pid_list), Z},
(Tells process X to send a message to process Z with a delay of Y ms)

lists:nth(X,Pid_list) ! {order_send_msg, lists:nth(Y,Pid_list), Y}
(Tells process X to send a message to process Y)

lists:nth(X,Pid_list) ! {give_id_and_matrix}
(The process number X prints its matrix on the standard output)

lists:nth(X,Pid_list) ! {order_event} 
(The process number X consider a local event, 
unrelated with other processes and update its matrix)

timer:sleep(X) 
(the supervisor sleeps for X ms)



**exercise_5(Pid_list)**
>To show the correct execution of the program.
The events are those from the fith exercise 
of the first exercise sheet. For more, see 
the file "td1-tps-logique-correction.pdf"

list Pid_list : contains the PIDs of each running process


### Module node.erl

**do_something(N, M)**
>This function is executed by each process when starting (apart from the supervisor).
It receives the messages sent by the supervisor and by other processes.
It uses the algorithm of matrix clock synchronization to update the
clock of the process executing it.

int N : identifier of the process executing the function.
int M : total number of processes.


**process_store_stamp(Store_stamp, N, Clock, M)**
>This function parses the list Store_stamp, it contains the messages that couldn't
be delivered since they didn't met the condition specified by the algorithm.

list Store_stamp : contains the message that do not respect the causal order.
int N : identifier of the current process.
array Clock : it is the local clock of the current process.
int M : total number of process.


**update_mat_send(N, Num_dest, M, Mat)**
>Updates the local clock when a message is sent

int N : the Id of the current process.
int Num_dest : the Id of the receiver.
int M : the number of running processes.
int Mat : the local clock.


**check_mat_recv(N_src, Stamp, N, Clock, M)**
>checks if the conditions stated in the matrix clock synchronization algorithm
are respected by comparing the local clock and the Stamp of the message. If not,
the message is stored in a list and awaits the proper time to be delivered

int N_src : the Id of the sender.
int Stamp : the matrix received with the message.
int N : the Id of the receiver(local process).
int Mat : the local clock.
int M : the number of running processes.


**update_mat_recv(N_src, Stamp, N, Clock, M)**
>updates the local clock if the conditions stated in the matrix clock 
synchronization algorithm are respected.

int N_src : the Id of the sender.
int Stamp : the matrix received with the message.
int N : the Id of the receiver(local process).
int Mat : the local clock.
int M : the number of running processes.


**update_mat_event(N, M, Mat)**
>Updates the clock of the current process by considering a local event.

int N : the Id of the receiver(local process).
int Mat : the local clock.
int M : the number of running processes.


**print_matrix(N, M, Mat)**
>Prints the local clock of the current process on the standard output.

int N : the Id of the receiver(local process).
int Mat : the local clock.
int M : the number of running processes.


**print_pid_list(Pid_list)**
>Prints the list containing the PIDs of all processes (except the supervisor)

list Pid_list


