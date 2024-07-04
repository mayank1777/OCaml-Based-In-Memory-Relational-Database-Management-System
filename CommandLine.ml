#use "DefTypeAndFunctions.ml"
open Printf

let rec add_row_with_user_input dataBase tbTitle =
  let table = fetchTable dataBase.dataBaseTableRecord tbTitle in
  let rec add_col_content specifyColumn accumalator =
    match specifyColumn with
    | [] -> accumalator
    | coldef :: rest ->
        printf "Enter content for column '%s': " coldef.colName;
        let input_content = read_line () in
        let col_content =
          match coldef.columnEntryType with
          | INT -> Int (int_of_string input_content)
          | CHAR _ -> Char input_content
        in
        add_col_content rest ({ columnTitle = coldef.colName; content = col_content } :: accumalator)
  in
  let new_row_data = add_col_content table.specifyColumn [] in
  let dataBase' = insertRowData dataBase tbTitle { data = List.rev new_row_data } in
  printf "Row added to table '%s'.\n" tbTitle;
  dataBase'


let rec filter_rows_with_user_input dataBase tbTitle =
    printf "Choose a column to filter by:\n";
    let table = fetchTable dataBase.dataBaseTableRecord tbTitle in
    displayColumnSpecificationList table.specifyColumn;
    printf "Enter column name: ";
    let colname = read_line () in
    let coldef = List.find (fun c -> c.colName = colname) table.specifyColumn in
    printf "Enter content: ";
    let input_content = read_line () in
    let col_content =
      match coldef.columnEntryType with
      | INT -> Int (int_of_string input_content)
      | CHAR _ -> Char input_content
    in
    printf "Choose filterType operator:\n";
    printf "1 - Equal (OperatorEqual)\n";
    printf "2 - Not Equal (OperatorNotEqual)\n";
    printf "Enter your choice (1 or 2): ";
    let filterTypeOperator_choice = read_int () in
    let filterTypeOperator =
      match filterTypeOperator_choice with
      | 1 -> OperatorEqual
      | 2 -> OperatorNotEqual
      | _ -> failwith "Invalid choice"
    in
    let acontent = { columnTitle = colname; content = col_content } in
    let filterType = { filterTypeData = acontent; filterTypeOperator = filterTypeOperator } in
    let filteredRowlist = selectEntries filterType table in
    displayFilterList table.specifyColumn filteredRowlist;
    dataBase


let rec update_row_with_user_input dataBase tbTitle =
      let table = fetchTable dataBase.dataBaseTableRecord tbTitle in
      printf "Enter column name to be modified: ";
      let filter_colname = read_line () in
      let coldef =
        match List.find_opt (fun c -> c.colName = filter_colname) table.specifyColumn with
        | Some coldef -> coldef
        | None -> failwith "Column not found"
      in
      printf "Enter content for filterType: ";
      let input_content = read_line () in
      let filter_content =
        match coldef.columnEntryType with
        | INT -> Int (int_of_string input_content)
        | CHAR _ -> Char input_content
      in
      printf "Choose filterType operator:\n";
      printf "1 - Equal (OperatorEqual)\n";
      printf "2 - Not Equal (OperatorNotEqual)\n";
      printf "Enter your choice (1 or 2): ";
      let filterTypeOperator_choice = read_int () in
      let filterTypeOperator =
        match filterTypeOperator_choice with
        | 1 -> OperatorEqual
        | 2 -> OperatorNotEqual
        | _ -> failwith "Invalid choice"
      in
      let filtercontent = { columnTitle = filter_colname; content = filter_content } in
      let filterType = { filterTypeData = filtercontent; filterTypeOperator = filterTypeOperator } in
      printf "Enter column name in which  update has to be done: ";
      let update_colname = read_line () in
      let update_coldef =
        match List.find_opt (fun c -> c.colName = update_colname) table.specifyColumn with
        | Some coldef -> coldef
        | None -> failwith "Column not found"
      in
      printf "Enter content for update: ";
      let update_input_content = read_line () in
      let update_col_content =
        match update_coldef.columnEntryType with
        | INT -> Int (int_of_string update_input_content)
        | CHAR _ -> Char update_input_content
      in
      let newEntry = { columnTitle = update_colname; content = update_col_content } in
      let dataBase' = modifyRows dataBase tbTitle filterType newEntry in
      printf "Rows modified in table '%s'.\n" tbTitle;
      dataBase'
    
let rec delete_rows_with_user_input dataBase tbTitle =
      printf "Choose a column to filter by:\n";
      let table = fetchTable dataBase.dataBaseTableRecord tbTitle in
      displayColumnSpecificationList table.specifyColumn;
      printf "Enter column name: ";
      let colname = read_line () in
      let coldef = List.find (fun c -> c.colName = colname) table.specifyColumn in
      printf "Enter content: ";
      let input_content = read_line () in
      let col_content =
        match coldef.columnEntryType with
        | INT -> Int (int_of_string input_content)
        | CHAR _ -> Char input_content
      in
      printf "Choose filterType operator:\n";
      printf "1 - Equal (OperatorEqual)\n";
      printf "2 - Not Equal (OperatorNotEqual)\n";
      printf "Enter your choice (1 or 2): ";
      let filterTypeOperator_choice = read_int () in
      let filterTypeOperator =
        match filterTypeOperator_choice with
        | 1 -> OperatorEqual
        | 2 -> OperatorNotEqual
        | _ -> failwith "Invalid choice"
      in
      let acontent = { columnTitle = colname; content = col_content } in
      let filterType = { filterTypeData = acontent; filterTypeOperator = filterTypeOperator } in
      let dataBase' = removeRow dataBase filterType tbTitle in
      printf "Rows deleted from table '%s'.\n" tbTitle;
      dataBase'
      
let rec join_tables_with_user_input dataBase =
        printf "Enter the first table name: ";
        let table1Name = read_line () in
        printf "Enter the second table name: ";
        let table2Name = read_line () in
        printf "Enter the column name to join on: ";
        let joinColName = read_line () in
        printf "Enter the name for the joined table: ";
        let joinedtbTitle = read_line () in
        let dataBase' = joinTables dataBase table1Name table2Name joinColName joinedtbTitle in
        printf "Tables '%s' and '%s' joined.\n" table1Name table2Name;
        dataBase'

(* Define the data types for user choices *)
type user_choice =
  | ConstructTable
  | DropTable
  | AddColumnToTable
  | AddRow
  | FilterRows
  | DeleteRows
  | UpdateRows
  | PrintTable
  | JoinTables
  | Quit

(* Define the function to print the menu of choices *)
let print_menu () =
  printf "Choose an option:\n";
  printf "1 - Create table\n";
  printf "2 - Drop table\n";
  printf "3 - Add column to table\n";
  printf "4 - Add row(s) to table\n";
  printf "5 - Filter rows\n";
  printf "6 - Delete rows\n";
  printf "7 - Update rows\n";
  printf "8 - Print table\n";
  printf "9 - Join tables\n";
  printf "10 - Quit\n"

(* Define the function to get user input *)
let rec get_user_choice () =
  printf "> ";
  match read_line () with
  | "1" -> ConstructTable
  | "2" -> DropTable
  | "3" -> AddColumnToTable
  | "4" -> AddRow
  | "5" -> FilterRows
  | "6" -> DeleteRows
  | "7" -> UpdateRows
  | "8" -> PrintTable
  | "9" -> JoinTables
  | "10" -> Quit
  | _ -> printf "Invalid choice. Please try again.\n"; get_user_choice ()

(* Define the function to print table names and column definitions *)
let print_tables_and_columns dataBase =
  printf "..................................................\n";
  printf "..................................................\n";
  printf "Tables in database %s:\n" dataBase.dataBaseTitle;
  List.iter (fun table ->
    printf "Table: %s\n" table.tbTitle;
    printf "Columns:\n";
    List.iter (fun coldef ->
      printf "- %s\n" coldef.colName
    ) table.specifyColumn;
    printf "..................................................\n"
  ) dataBase.dataBaseTableRecord


(* Define the main function for user interaction *)
let rec main_loop dataBase =
  print_tables_and_columns dataBase;
  print_menu ();
  match get_user_choice () with
  | ConstructTable -> (* Implement create table functionality here *)
      printf "Enter table name: ";
      let tbTitle = read_line () in
      let dataBase' = constructTable dataBase tbTitle in
      printf "Table '%s' created.\n" tbTitle;
      main_loop dataBase'
  | DropTable -> (* Implement drop table functionality here *)
      printf "Enter table name to drop: ";
      let tbTitle = read_line () in
      let dataBase' = removeTable dataBase tbTitle in
      printf "Table '%s' dropped.\n" tbTitle;
      main_loop dataBase'
  | AddColumnToTable -> 
      printf "Enter table name to add column to: ";
      let tbTitle = read_line () in
      printf "Enter column name: ";
      let colname = read_line () in
      printf "Select column type:\n";
      printf "1 - INT\n";
      printf "2 - CHAR 30\n";
      printf "Enter your choice (1 or 2): ";
      let columnEntryType_choice = read_int () in
      let columnEntryType =
        match columnEntryType_choice with
        | 1 -> INT
        | 2 -> CHAR 30
        | _ -> failwith "Invalid choice"
      in
      let colDef = { colName = colname; columnEntryType = columnEntryType } in
      let dataBase' = insertColumnToTable dataBase tbTitle colDef in
      printf "Column '%s' added to table '%s'.\n" colname tbTitle;
      main_loop dataBase'
  | AddRow -> (* Implement add row functionality here *)
      printf "Enter table name to add row to: ";
      let tbTitle = read_line () in
      let dataBase' = add_row_with_user_input dataBase tbTitle in
      main_loop dataBase'
  | FilterRows ->
      printf "Enter table name to filter rows: ";
      let tbTitle = read_line () in
      let dataBase' = filter_rows_with_user_input dataBase tbTitle in
      main_loop dataBase'
  | DeleteRows -> 
      printf "Enter table name to delete rows: ";
      let tbTitle = read_line () in
      let dataBase' = delete_rows_with_user_input dataBase tbTitle in
      main_loop dataBase'
  | UpdateRows -> (* Implement update rows functionality here *)
      printf "Enter table name in which you want to perform the update: ";
      let tbTitle = read_line () in
      let dataBase' = update_row_with_user_input dataBase tbTitle in
      main_loop dataBase'
  | PrintTable -> (* Implement print table functionality here *)
      printf "Enter table name to print: ";
      let tbTitle = read_line () in
      displayTable dataBase tbTitle;
      main_loop dataBase
  | JoinTables -> (* Implement join tables functionality here *)
      let dataBase' = join_tables_with_user_input dataBase in 
      main_loop dataBase'
  | Quit -> printf "Exiting...\n";;

(* Test the user interface *)
let () =
  let dataBase = constructDataBase "SampleDatabase" in
  main_loop dataBase
