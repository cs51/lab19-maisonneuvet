(*
                Component Behaviors of an ATM Machine

The functions here represent the component behaviors that an ATM
machine can take, including: prompting for and acquiring from the
customer some information (choosing an action or entering an account
id or an amount); presenting information to the customer; dispensing
cash.

Implementation of these behaviors is likely to require some database
of accounts, each with an id number, a customer name, and a current
balance.
 *)

(* Customer account identifiers *)
type id = int

(* Possible actions that an ATM customer can perform *)
type action =
  | Balance           (* balance inquiry *)
  | Withdraw of int   (* withdraw an amount *)
  | Deposit of int    (* deposit an amount *)
  | Next              (* finish this customer and move on to the next one *)
  | Finished          (* shut down the ATM and exit entirely *)
;; 

(*....................................................................
 Initializing database of accounts
*)

(* A specification of a customer name and initial balance for
   initializing the account database *)
type account_spec = {name : string; id : id; balance : int} ;;

(* initialize accts -- Establishes a database of accounts, each with a
   name, aribtrary id, and balance. The names and balances are
   initialized as per the `accts` provided. *)

type mutable_acc = {name : string; id : id; mutable balance : int} ;;

type db = mutable_acc list

let database : db ref = ref [] ;;

let initialize (lst : account_spec list) : unit =
   let rec init (l : account_spec list) : db =
      match l with
      | [] -> []
      | {name=n; id=i; balance=b} :: tl -> {name=n; id =i; balance=b} :: init tl in
      database := init lst;;

(*....................................................................
 Acquiring information from the customer
*)

(* acquire_id () -- Requests from the ATM customer and returns an id
   (akin to entering one's ATM card), by prompting for an id number
   and reading an id from stdin. *)
let acquire_id : id = 
   Printf.printf ("Enter customer id: ");
   read_int () ;;
   

(* acquire_amount () -- Requests from the ATM customer and returns an
   amount by prompting for an amount and reading an int from stdin. *)
let acquire_amount =
  Printf.printf "Enter amount: "; 
  read_int () ;;

(* acquire_act () -- Requests from the user and returns an action to
   be performed, as a value of type action *)
let acquire_act =
  Printf.printf "Enter action: (B) Balance (-) Withdraw (+) Deposit (=) Done (X) Exit: "; 
  match read_line () with 
  | "B" -> Balance
  | "-" -> Withdraw acquire_amount
  | "+" -> Deposit acquire_amount
  | "=" -> Next
  | "X" -> Finished
  | _ -> failwith "require action" ;;

(*....................................................................
  Querying and updating the account database

  These functions all raise Not_found if there is no account with the
  given id. 
 *)
  
(* get_balance id -- Returns the balance for the customer account with
   the given id. *)
let get_balance : id -> int =
  let aux (id : id) =
    match (List.filter (fun acc -> acc.id = id) !database) with
    | [] -> raise (Invalid_argument "ID does not exist")
    | hd :: _ -> hd.balance
  in aux ;;

(* get_name id -- Returns the name associated with the customer
   account with the given id. *)
let get_name : id -> string =
  let aux (id : id) =
    match (List.filter (fun acc -> acc.id = id) !database) with
    | [] -> raise (Invalid_argument "ID does not exist")
    | hd :: _ -> hd.name 
   in aux ;;

(* update_balance id amount -- Modifies the balance of the customer
   account with the given id,setting it to the given amount. *)
let update_balance (id : id) (amount : int) : unit =
  if List.exists (fun x -> x.id = id) (!database) then 
    let account = List.find (fun x -> x.id = id) !database in
    account.balance <- amount
  else raise Not_found ;;

(*....................................................................
  Presenting information and cash to the customer
 *)
  
(* present_message message -- Presents to the customer (on stdout) the
   given message followed by a newline. *)
let present_message (s : string) : unit =
  Printf.printf "%s/n" s ;;

(* deliver_cash amount -- Dispenses the given amount of cash to the
   customer (really just prints to stdout a message to that
   effect). *)
let deliver_cash (cash : int) : unit =
  Printf.printf "%i/n" cash ;;
