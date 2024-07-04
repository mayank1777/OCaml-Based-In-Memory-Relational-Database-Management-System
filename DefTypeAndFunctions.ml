(*** DATA TYPE DEFINITIONS **)

(* Relational (comparison) operators *)
type operatorType = 
| OperatorLessThan 
| OperatorGreaterThan 
| OperatorGreaterEqual 
| OperatorLessEqual
| OperatorEqual
| OperatorNotEqual;;


(* Variant type to store a content *)
type columnEntrycontentType = 
|Char of string
| Int of int ;;


type joinType =
| InnerJoin
| LeftJoin
| RightJoin



(* Supported SQL Column Types *)
type columnEntryType = 
|CHAR of int 
| INT;;

(* Record to store a column definition *)
type columnSpecification = 
{ 
  columnEntryType : columnEntryType;
  colName : string;
};;
      

type transactionStatus =
| Pending
| Committed
| RolledBack


type transactionStatus =
| Pending
| Committed
| RolledBack


(* Record type to store a content for a given column in table *)                           
type columnContent = 
{
  content : columnEntrycontentType;
  columnTitle : string
};;

(* filterType structure *)
type filterType = 
{ 
  filterTypeOperator : operatorType ; 
  filterTypeData : columnContent
};;

type constraintType =
| PrimaryKey of string list  (* Primary key constraint *)
| ForeignKey of string * string  (* Foreign key constraint *)
| Unique of string list  (* Unique constraint *)
| NotNull of string  (* Not null constraint *)


(* Record type to model a row in a table *)
type singleRowEntry = 
{
  data : columnContent list
};;


(* Record type to maintain a list of tables in dataBase *)
type table = 
{
  rowEntries : singleRowEntry list;
  specifyColumn : columnSpecification list; 
  tbTitle : string
};;

(* Record type to represent the entire database *)
type dataBase = 
{ 
  dataBaseTableRecord : table list;
  dataBaseTitle : string
};;  




let computeBool val1 val2 operator = match operator with
| OperatorEqual -> val1 = val2
| OperatorNotEqual -> val1 <> val2
| OperatorGreaterThan -> val1 > val2 
| OperatorLessThan -> val1 < val2
| OperatorLessEqual -> val1 <= val2
| OperatorGreaterEqual -> val1 >= val2;;


let invertfilterType filter = match filter.filterTypeOperator with
| OperatorEqual -> {filter with filterTypeOperator=OperatorNotEqual}
| OperatorNotEqual -> {filter with filterTypeOperator=OperatorEqual}
| OperatorGreaterThan -> {filter with filterTypeOperator=OperatorLessEqual}
| OperatorLessThan -> {filter with filterTypeOperator=OperatorGreaterEqual}
| OperatorLessEqual -> {filter with filterTypeOperator=OperatorGreaterThan}
| OperatorGreaterEqual -> {filter with filterTypeOperator=OperatorLessThan};;



(* updating dataBaseTableRecord *)
let replaceTable dataBaseTableRecord table accumulator =
    let replace_if_match acc record =
       if record.tbTitle = table.tbTitle then
         table :: acc
       else
         record :: acc
    in
List.rev (List.fold_left replace_if_match [] dataBaseTableRecord)
            

let fetchTable dataBaseTableRecord tbTitle =
  try
    List.find (fun table -> table.tbTitle = tbTitle) dataBaseTableRecord
  with
  | Not_found -> failwith ("Table Not Found: " ^ tbTitle)


(* check if some entry in row statisfy that filter and return bool  *)
let checkfilterType p singleRowEntry =
  let filterType_matches col =
    col.columnTitle = p.filterTypeData.columnTitle &&
    computeBool col.content p.filterTypeData.content p.filterTypeOperator
  in
List.exists filterType_matches singleRowEntry
            

(* selecting only those rows in the table with satisfy the filter *)
let selectEntries filter table =
  let filterType_matches row =
    checkfilterType filter row.data
  in
  List.filter filterType_matches table.rowEntries





(* ROW DATA RETRIEVAL *)
(* return the content in row for a given col *)
let retriveSingleEntery coldef singleRowEntry =  
  let matching_column row_data = row_data.columnTitle = coldef.colName in
  (* return the content in case we found, otherwise return "NULL" indicating no such col in given row *)
  match List.find_opt matching_column singleRowEntry with
  | Some column -> column.content
  | None -> Char "NULL"
  


(* DISPLAY FUNCTIONS *)


  (* call the above function on every row *)
let displayRowEntries specifyColumn rowEntries =
  let displayRowInformation specifyColumn singleRowEntry =
    let toString v = match v with | Int i -> string_of_int i | Char s -> s in
    List.iter (fun c -> Printf.printf "%-30s" (toString (retriveSingleEntery c singleRowEntry))) specifyColumn;
    print_newline() in
    List.iter (fun row -> displayRowInformation specifyColumn row.data) rowEntries


let displayColumnSpecificationList specifyColumn =
  List.iter (fun c -> Printf.printf "%-30s" c.colName) specifyColumn;
  print_newline()


(* ROWS *)

(* it is used to print the filterred rows *)
let displayFilterList specifyColumn rowEntries =
  let separator = "=========================================================================\n" in
  displayColumnSpecificationList specifyColumn;
  print_string separator;
  displayRowEntries specifyColumn rowEntries;
  Printf.printf "%d Rows Printed\n\n" (List.length rowEntries)
  

let displayTable dataBase tbTitle =
  let displayHeader tbTitle =
    Printf.printf "\nTABLE: %s\n" tbTitle;
  in
  
  let displaySeparator () =
    print_string "========================================================================\n";
  in
  
  let table = fetchTable dataBase.dataBaseTableRecord tbTitle in
  displayHeader tbTitle;
  displayColumnSpecificationList table.specifyColumn;
  displaySeparator ();
  displayRowEntries table.specifyColumn table.rowEntries;
  Printf.printf "%d Rows displayed\n\n" (List.length table.rowEntries);
  print_newline ();
  dataBase
  


let displayAllTablesNames dataBase =
  print_endline ("Tables in database " ^ dataBase.dataBaseTitle ^ ":");
  List.iter (fun table -> print_endline table.tbTitle) dataBase.dataBaseTableRecord;;



(* DATABASE CREATION *)

let constructDataBase name = 
  { dataBaseTitle = name; dataBaseTableRecord = [] };;



(* TABLE CREATION *)

let constructTable dataBase tbTitle =
    { dataBase with dataBaseTableRecord = { tbTitle = tbTitle; specifyColumn = []; rowEntries = [] } :: dataBase.dataBaseTableRecord }
  

let removeTable dataBase tbTitle =
  let filterdataBaseTableRecord dataBaseTableRecord = List.filter (fun entry -> entry.tbTitle <> tbTitle) dataBaseTableRecord in
  let newdataBaseTableRecord = filterdataBaseTableRecord dataBase.dataBaseTableRecord in
  { dataBase with dataBaseTableRecord = newdataBaseTableRecord }
  

let insertRowData dataBase tbTitle singleRowEntry =
      let modifyTable table = { table with rowEntries = table.rowEntries @ [singleRowEntry] } in
      let table = fetchTable dataBase.dataBaseTableRecord tbTitle in
      let modifiedTable = modifyTable table in
      let newdataBaseTableRecord = replaceTable dataBase.dataBaseTableRecord modifiedTable [] in
      { dataBase with dataBaseTableRecord = newdataBaseTableRecord }
    
    

let insertColumnToTable dataBase tbTitle colDef =
    let table = fetchTable dataBase.dataBaseTableRecord tbTitle in
    let modifiedTable = { table with specifyColumn = table.specifyColumn @ [colDef] } in
    let newdataBaseTableRecord = replaceTable dataBase.dataBaseTableRecord modifiedTable [] in
    { dataBase with dataBaseTableRecord = newdataBaseTableRecord }
    




(* Update *)

(* replace entry in a row with a newEntry *)
let replaceRowEntryWithVal singleRowEntry newEntry =
  (* check wheather entry is consistent with newEntry *)
  let replaceSingleEntryWithVal columnContent newEntry =
    if columnContent.columnTitle = newEntry.columnTitle then
      { columnContent with content = newEntry.content }
    else
      columnContent
  in
  (* return the updated rowData *)
  let replacesingleRowEntry singleRowEntry newEntry =
    List.map (fun rd -> replaceSingleEntryWithVal rd newEntry) singleRowEntry
  in
  (* update rowData with new row data *)
  { singleRowEntry with data = replacesingleRowEntry singleRowEntry.data newEntry }
  


(* replacing every entry in table which satify thegiven filter with newEntry *)
let modifyRows dataBase tbTitle filter newEntry =
  let table = fetchTable dataBase.dataBaseTableRecord tbTitle in
  let updateRow row =
    if checkfilterType filter row.data then
      replaceRowEntryWithVal row newEntry
    else
      row
  in
  let modified_rowEntries = List.map updateRow table.rowEntries in
  let modified_table = { table with rowEntries = modified_rowEntries } in
  let newdataBaseTableRecord = replaceTable dataBase.dataBaseTableRecord modified_table [] in
  { dataBase with dataBaseTableRecord = newdataBaseTableRecord }



(* DELETE *)

(* basically update a table , and in that table it it keeps only thosr rows wich does not satisfy that predicate *)
let removeRow dataBase filter tbTitle =
  (* gives table by deleting rows which satisfy the given filter *)
  let filtered_table =
    let table = fetchTable dataBase.dataBaseTableRecord tbTitle in
    let filterType_matches row =
      not (checkfilterType filter row.data)
    in
    { table with rowEntries = List.filter filterType_matches table.rowEntries }
  in
  let newdataBaseTableRecord =
    replaceTable dataBase.dataBaseTableRecord filtered_table []
  in
  { dataBase with dataBaseTableRecord = newdataBaseTableRecord }
  

(* JOIN *)

  let checkfilterTypeForJoin colname row1 row2 =
    let rec find_column_content colname row_data =
      match row_data with
      | [] -> failwith "Column not found, Join Can not Be Performed! "
      | col :: rest ->
          if col.columnTitle = colname then
            col.content
          else
            find_column_content colname rest
    in
    let content1 = find_column_content colname row1 in
    let content2 = find_column_content colname row2 in
    content1 = content2

  
  
    let joinTables dataBase table1Name table2Name colname joinedtbTitle =
      let table1 = fetchTable dataBase.dataBaseTableRecord table1Name in
      let table2 = fetchTable dataBase.dataBaseTableRecord table2Name in
      
      (* Exclude colname from the column definition list of table2 *)
      let filteredspecifyColumn = List.filter (fun coldef -> coldef.colName <> colname) table2.specifyColumn in
      
      let rec cartesianProduct row1 row2 accumalator =
        match row1 with
        | [] -> accumalator
        | r1 :: rest1 ->
            let rec addCombinedRows row2 accumalator =
              match row2 with
              | [] -> accumalator
              | r2 :: rest2 ->
                  if checkfilterTypeForJoin colname r1.data r2.data then
                    let combinedRow = { data = r1.data @ r2.data } in
                    addCombinedRows rest2 (combinedRow :: accumalator)
                  else
                    addCombinedRows rest2 accumalator
            in
            let combinedRows = addCombinedRows table2.rowEntries [] in
            cartesianProduct rest1 table2.rowEntries (combinedRows @ accumalator)
      in
      
      let combinedRows = cartesianProduct table1.rowEntries table2.rowEntries [] in
      let joinedTable = { tbTitle = joinedtbTitle; specifyColumn = table1.specifyColumn @ filteredspecifyColumn; rowEntries = combinedRows } in
      let dataBase' = { dataBase with dataBaseTableRecord = dataBase.dataBaseTableRecord @ [joinedTable] } in
      dataBase'



