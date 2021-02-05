#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#include "cassandra.h"
#include "bindings.h"

const CassPrepared* prepared_insert_txid_outputs = NULL;
const CassPrepared* prepared_insert_tx= NULL;
const CassPrepared* prepared_insert_script_hash_outputs = NULL;
const CassPrepared* prepared_insert_script_hash_unspent_outputs = NULL;
const CassPrepared* prepared_delete_script_hash_unspent_outputs = NULL;
const CassPrepared* prepared_insert_script_output_protocol = NULL;

CassError prepare(CassSession* session, const CassPrepared** prepared, const char* query) {
  CassError rc = CASS_OK;
  CassFuture* future = NULL;

  future = cass_session_prepare(session, query);
  cass_future_wait(future);

  rc = cass_future_error_code(future);
  if (rc != CASS_OK) {
    print_error(future);
  } else {
    *prepared = cass_future_get_prepared(future);
  }

  cass_future_free(future);

  return rc;
}

void prepare_all(){
  const char* q_insert_txid_outputs = "INSERT INTO xoken.txid_outputs (txid, output_index,address,script_hash,is_recv,block_info,other,value) VALUES (?, ?, ?, ?, ?, ?, ?, ?);";
  const char* q_insert_tx = "INSERT INTO xoken.transactions (tx_id, block_info, tx_serialized, inputs, fees) VALUES (?, ?, ?, ?, ?);";
  const char* q_insert_script_hash_outputs = "INSERT INTO xoken.script_hash_outputs (script_hash, nominal_tx_index, output) VALUES (?, ?, ?);";
  const char* q_insert_script_hash_unspent_outputs = "INSERT INTO xoken.script_hash_unspent_outputs (script_hash, output) VALUES (?, ?);";
  const char* q_delete_script_hash_unspent_outputs = "DELETE FROM xoken.script_hash_unspent_outputs WHERE script_hash = ? and output = ?;";
  const char* q_insert_script_output_protocol = "INSERT INTO xoken.script_output_protocol (proto_str, txid, fees, size, output_index, nominal_tx_index) VALUES (?, ?, ?, ?, ?, ?);";
  CassSession* sess = getSession();
  prepare(sess,&prepared_insert_txid_outputs,q_insert_txid_outputs);
  prepare(sess,&prepared_insert_tx,q_insert_tx);
  prepare(sess,&prepared_insert_script_hash_outputs,q_insert_script_hash_outputs);
  prepare(sess,&prepared_insert_script_hash_unspent_outputs,q_insert_script_hash_unspent_outputs);
  prepare(sess,&prepared_delete_script_hash_unspent_outputs,q_delete_script_hash_unspent_outputs);
  prepare(sess,&prepared_insert_script_output_protocol,q_insert_script_output_protocol);
}

void free_prepared(){
  cass_prepared_free(prepared_insert_txid_outputs);
  cass_prepared_free(prepared_insert_tx);
  cass_prepared_free(prepared_insert_script_hash_outputs);
  cass_prepared_free(prepared_insert_script_hash_unspent_outputs);
  cass_prepared_free(prepared_delete_script_hash_unspent_outputs);
  cass_prepared_free(prepared_insert_script_output_protocol);
}


int insert_txid_outputs( const char* txid
                       , int output_index
                       , const char* address
                       , const char* scripthash
                       , bool is_recv
                       , const char* block_infot, int block_infoi, int block_infoi1
                       , int other_len
                       , const char* other0[], int other0i[], int other1[], const char* other2[], long other2i[]
                       , long value){ // Maybe Bool, Int32, Maybe Int64, Text
  CassError rc = CASS_OK;
  CassStatement* statement = NULL;
  CassSession* sess = getSession();
  printf("%p",sess);
  CassFuture* future;
  statement = cass_prepared_bind(prepared_insert_txid_outputs);

/* 
CREATE TABLE xoken.txid_outputs (
    txid text,
    output_index int,
    address text,
    script_hash text,
    --is_recv boolean,
    --block_info frozen<tuple<text, int, int>>,
    --other set <frozen<tuple<frozen<tuple<text, int>>, int, frozen<tuple<text, bigint>>>>>,
    value bigint,
    PRIMARY KEY(txid, output_index, is_recv)
) WITH CLUSTERING ORDER BY (output_index asc);
*/

  CassTuple* block_info = NULL;
  block_info = cass_tuple_new(3);
  cass_tuple_set_string(block_info, 0, block_infot);
  cass_tuple_set_int32(block_info,1,(cass_int32_t)block_infoi);
  cass_tuple_set_int32(block_info,2,(cass_int32_t)block_infoi1);

  CassCollection* other = NULL;
  other = cass_collection_new(CASS_COLLECTION_TYPE_SET, other_len);

  CassTuple* ot1 = NULL;
  CassTuple* ot2 = NULL;
  CassTuple* ott = NULL;

  for(int i = 0; i < other_len; ++i){
  ot1 = cass_tuple_new(2);
  cass_tuple_set_string(ot1, 0, other0[i]);
  cass_tuple_set_int32(ot1,1,(cass_int32_t)other0i[i]);

  ot2 = cass_tuple_new(2);
  cass_tuple_set_string(ot2, 0, other2[i]);
  cass_tuple_set_int64(ot2,1,(cass_int64_t)other2i[i]);

  ott = cass_tuple_new(3);
  cass_tuple_set_tuple(ott, 0, ot1);
  cass_tuple_set_int32(ott,1,(cass_int32_t)other1[i]);
  cass_tuple_set_tuple(ott, 2, ot2);

  
  cass_collection_append_tuple(other,ott);
  }
  cass_statement_bind_string(statement, 0, txid);
  cass_statement_bind_int32(statement, 1, (cass_int32_t)output_index);
  cass_statement_bind_string(statement, 2, address);
  cass_statement_bind_string(statement, 3, scripthash);
  cass_statement_bind_bool(statement, 4, (cass_bool_t)is_recv);
  cass_statement_bind_tuple(statement, 5, block_info);
  cass_statement_bind_collection(statement, 6, other);
  cass_statement_bind_int64(statement, 7, (cass_int64_t)value);
  future = cass_session_execute(sess, statement);
  cass_future_wait(future);
  rc = cass_future_error_code(future);
  if (rc != CASS_OK) {
    print_error(future);
    cass_statement_free(statement);
    cass_future_free(future);
    return -1;
  }
  cass_statement_free(statement);

  cass_future_free(future);
  cass_tuple_free(block_info);
  cass_tuple_free(ot1);
  cass_tuple_free(ot2);
  cass_tuple_free(ott);
  cass_collection_free(other);

  return 0;
}

struct TxIdOutputsResult_ {
  const char* address;
  size_t address_len;
  const char* script_hash;
  size_t script_hash_len;
  cass_int64_t value;
};

typedef struct TxIdOutputsResult_ TxIdOutputsResult;

TxIdOutputsResult* select_txid_outputs( const char* txid, int output_index){
  CassError rc = CASS_OK;
  CassStatement* statement = NULL;
  CassSession* sess = getSession();
  const char* query = "SELECT address, script_hash, value FROM xoken.txid_outputs WHERE txid=? AND output_index=? AND is_recv=true;";
  CassFuture* future;
  statement = cass_statement_new(query, 2);
  TxIdOutputsResult* res = (TxIdOutputsResult*)(malloc(sizeof(TxIdOutputsResult)));
/* 
CREATE TABLE xoken.txid_outputs (
    txid text,
    output_index int,
    address text,
    script_hash text,
    --is_recv boolean,
    --block_info frozen<tuple<text, int, int>>,
    --other set <frozen<tuple<frozen<tuple<text, int>>, int, frozen<tuple<text, bigint>>>>>,
    value bigint,
    PRIMARY KEY(txid, output_index, is_recv)
) WITH CLUSTERING ORDER BY (output_index asc);
*/

  cass_statement_bind_string(statement, 0, txid);
  cass_statement_bind_int32(statement, 1, (cass_int32_t)output_index);
  future = cass_session_execute(sess, statement);
  cass_future_wait(future);
  rc = cass_future_error_code(future);
  if (rc != CASS_OK) {
    print_error(future);
    cass_future_free(future);
    return NULL;
  } else {
    const CassResult* result = cass_future_get_result(future);
    CassIterator* iterator = cass_iterator_from_result(result);

    if (cass_iterator_next(iterator)) {
      const CassRow* row = cass_iterator_get_row(iterator);
      cass_value_get_string(cass_row_get_column(row, 0), &res->address, &res->address_len);
      cass_value_get_string(cass_row_get_column(row, 1), &res->script_hash, &res->script_hash_len);
      cass_value_get_int64(cass_row_get_column(row, 2), &res->value);
        printf("%d\n",res->value);
  printf("%s\n",res->address);
  printf("%d\n",res->address_len);
  printf("%d\n",res->script_hash_len);
  printf("%s\n",res->script_hash);
    }
    else
      return NULL;

    cass_result_free(result);
    cass_iterator_free(iterator);
  }

  cass_future_free(future);
  cass_statement_free(statement);
  //return NULL;
  return res;
}

int insert_tx( const char* tx_id
             , const char* block_infot, int block_infoi, int block_infoi1
             , int tx_serialized_len
             , const uint8_t* tx_serialized
             , int input_len
             , const char* input0[], int input0i[], int input1[], const char* input2[], long input2i[]
             , long fees){ // Maybe Bool, Int32, Maybe Int64, Text
  CassError rc = CASS_OK;
  CassStatement* statement = NULL;
  CassSession* sess = getSession();
  printf("%p",sess);
  CassFuture* future;
  statement = cass_prepared_bind(prepared_insert_tx);

/* 
CREATE TABLE xoken.transactions (
    tx_id text PRIMARY KEY,
    block_info frozen<tuple<text, int, int>>,
    tx_serialized blob,
    inputs set  <frozen<tuple<frozen<tuple<text, int>>, int, frozen<tuple<text, bigint>>>>>,
    fees bigint
);
*/

  CassTuple* block_info = NULL;
  block_info = cass_tuple_new(3);
  cass_tuple_set_string(block_info, 0, block_infot);
  cass_tuple_set_int32(block_info,1,(cass_int32_t)block_infoi);
  cass_tuple_set_int32(block_info,2,(cass_int32_t)block_infoi1);

  CassCollection* inputs = NULL;
  inputs = cass_collection_new(CASS_COLLECTION_TYPE_SET, input_len);

  CassTuple* ot1 = NULL;
  CassTuple* ot2 = NULL;
  CassTuple* ott = NULL;

  for(int i = 0; i < input_len; ++i){
  ot1 = cass_tuple_new(2);
  cass_tuple_set_string(ot1, 0, input0[i]);
  cass_tuple_set_int32(ot1,1,(cass_int32_t)input0i[i]);

  ot2 = cass_tuple_new(2);
  cass_tuple_set_string(ot2, 0, input2[i]);
  cass_tuple_set_int64(ot2,1,(cass_int64_t)input2i[i]);

  ott = cass_tuple_new(3);
  cass_tuple_set_tuple(ott, 0, ot1);
  cass_tuple_set_int32(ott,1,(cass_int32_t)input1[i]);
  cass_tuple_set_tuple(ott, 2, ot2);

  
  cass_collection_append_tuple(inputs,ott);
  }
  cass_statement_bind_string(statement, 0, tx_id);
  cass_statement_bind_tuple(statement, 1, block_info);
  cass_statement_bind_bytes(statement, 2, tx_serialized, tx_serialized_len);
  cass_statement_bind_collection(statement, 3, inputs);
  cass_statement_bind_int64(statement, 4, (cass_int64_t)fees);
  future = cass_session_execute(sess, statement);
  cass_future_wait(future);
  rc = cass_future_error_code(future);
  if (rc != CASS_OK) {
    print_error(future);
    cass_statement_free(statement);
    cass_future_free(future);
    return -1;
  }
  cass_statement_free(statement);

  cass_future_free(future);
  cass_tuple_free(block_info);
  cass_tuple_free(ot1);
  cass_tuple_free(ot2);
  cass_tuple_free(ott);
  cass_collection_free(inputs);

  return 0;
}


int insert_script_hash_outputs( const char* script_hash
                              , long nominal_tx_index
                              , const char* output_hash
                              , int output_index)
{ 
  CassError rc = CASS_OK;
  CassStatement* statement = NULL;
  CassSession* sess = getSession();
  printf("%p",sess);
  CassFuture* future;
  statement = cass_prepared_bind(prepared_insert_script_hash_outputs);

/* 
    script_hash text,
    nominal_tx_index bigint,
    output frozen<tuple<text, int>>
*/

  CassTuple* output = NULL;
  output = cass_tuple_new(2);
  cass_tuple_set_string(output, 0, output_hash);
  cass_tuple_set_int32(output,1,(cass_int32_t)output_index);

  cass_statement_bind_string(statement, 0, script_hash);
  cass_statement_bind_int64(statement, 1, (cass_int64_t)nominal_tx_index);
  cass_statement_bind_tuple(statement, 2, output);
  future = cass_session_execute(sess, statement);
  cass_future_wait(future);
  rc = cass_future_error_code(future);
  if (rc != CASS_OK) {
    print_error(future);
    cass_statement_free(statement);
    cass_future_free(future);
    return -1;
  }

  cass_statement_free(statement);
  cass_future_free(future);
  cass_tuple_free(output);

  return 0;
}


int insert_script_hash_unspent_outputs( const char* script_hash
                                      , const char* output_hash
                                      , int output_index)
{ 
  CassError rc = CASS_OK;
  CassStatement* statement = NULL;
  CassSession* sess = getSession();
  printf("%p",sess);
  CassFuture* future;
  statement = cass_prepared_bind(prepared_insert_script_hash_unspent_outputs);

/* 
    script_hash text,
    output frozen<tuple<text, int>>
*/

  CassTuple* output = NULL;
  output = cass_tuple_new(2);
  cass_tuple_set_string(output, 0, output_hash);
  cass_tuple_set_int32(output,1,(cass_int32_t)output_index);

  cass_statement_bind_string(statement, 0, script_hash);
  cass_statement_bind_tuple(statement, 1, output);
  future = cass_session_execute(sess, statement);
  cass_future_wait(future);
  rc = cass_future_error_code(future);
  if (rc != CASS_OK) {
    print_error(future);
    cass_statement_free(statement);
    cass_future_free(future);
    return -1;
  }

  cass_future_free(future);
  cass_statement_free(statement);
  cass_tuple_free(output);

  return 0;
}

int delete_script_hash_unspent_outputs( const char* script_hash
                                      , const char* output_hash
                                      , int output_index)
{ 
  CassError rc = CASS_OK;
  CassStatement* statement = NULL;
  CassSession* sess = getSession();
  printf("%p",sess);
  CassFuture* future;
  statement = cass_prepared_bind(prepared_delete_script_hash_unspent_outputs);

/* 
    script_hash text,
    output frozen<tuple<text, int>>
*/

  CassTuple* output = NULL;
  output = cass_tuple_new(2);
  cass_tuple_set_string(output, 0, output_hash);
  cass_tuple_set_int32(output,1,(cass_int32_t)output_index);

  cass_statement_bind_string(statement, 0, script_hash);
  cass_statement_bind_tuple(statement, 1, output);
  future = cass_session_execute(sess, statement);
  cass_future_wait(future);
  rc = cass_future_error_code(future);
  if (rc != CASS_OK) {
    print_error(future);
    cass_statement_free(statement);
    cass_future_free(future);
    return -1;
  }

  cass_future_free(future);
  cass_statement_free(statement);
  cass_tuple_free(output);

  return 0;
}

int insert_script_output_protocol( const char* proto_str
                                 , const char* txid
                                 , int output_index
                                 , long fees
                                 , int size
                                 , long nominal_tx_index)
{ 
  CassError rc = CASS_OK;
  CassStatement* statement = NULL;
  CassSession* sess = getSession();
  printf("%p",sess);
  CassFuture* future;
  statement = cass_prepared_bind(prepared_insert_script_output_protocol);

/* 
    proto_str text,
    txid text,
    fees bigint,
    size int,
    output_index int,
    nominal_tx_index bigint
*/

  cass_statement_bind_string(statement, 0, proto_str);
  cass_statement_bind_string(statement, 1, txid);
  cass_statement_bind_int64(statement, 2, (cass_int64_t)fees);
  cass_statement_bind_int32(statement, 3, (cass_int32_t)size);
  cass_statement_bind_int32(statement, 4, (cass_int32_t)output_index);
  cass_statement_bind_int64(statement, 5, (cass_int64_t)nominal_tx_index);

  future = cass_session_execute(sess, statement);
  cass_future_wait(future);
  rc = cass_future_error_code(future);
  if (rc != CASS_OK) {
    print_error(future);
    cass_statement_free(statement);
    cass_future_free(future);
    return -1;
  }

  cass_statement_free(statement);
  cass_future_free(future);

  return 0;
}
