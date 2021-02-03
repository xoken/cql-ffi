#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#include "cassandra.h"
#include "bindings.h"

int insert_txid_outputs( const char* txid
                       , int output_index
                       , const char* address
                       , const char* scripthash
                       , bool is_recv
                       , const char* block_infot, int block_infoi, int block_infoi1
                       , const char* other0, int other0i, int other1, const char* other2, long other2i
                       , long value){ // Maybe Bool, Int32, Maybe Int64, Text
  CassError rc = CASS_OK;
  CassStatement* statement = NULL;
  CassSession* sess = getSession();
  printf("%p",sess);
  const char* query = "INSERT INTO xoken.txid_outputs (txid, output_index,address,script_hash,is_recv,block_info,other,value) VALUES (?, ?, ?, ?, ?, ?, ?, ?);";
  CassFuture* future;
  statement = cass_statement_new(query, 8);

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

  CassTuple* ot1 = NULL;
  ot1 = cass_tuple_new(2);
  cass_tuple_set_string(ot1, 0, other0);
  cass_tuple_set_int32(ot1,1,(cass_int32_t)other0i);

  CassTuple* ot2 = NULL;
  ot2 = cass_tuple_new(2);
  cass_tuple_set_string(ot2, 0, other2);
  cass_tuple_set_int64(ot2,1,(cass_int64_t)other2i);

  CassTuple* ott = NULL;
  ott = cass_tuple_new(3);
  cass_tuple_set_tuple(ott, 0, ot1);
  cass_tuple_set_int32(ott,1,(cass_int32_t)10);
  cass_tuple_set_tuple(ott, 2, ot2);

  CassCollection* other = NULL;
  other = cass_collection_new(CASS_COLLECTION_TYPE_SET, 1);
  cass_collection_append_tuple(other,ott);

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
    cass_future_free(future);
    return -1;
  }

  cass_future_free(future);
  cass_tuple_free(block_info);
  cass_tuple_free(ot1);
  cass_tuple_free(ot2);
  cass_tuple_free(ott);
  cass_collection_free(other);

  return 0;
}
