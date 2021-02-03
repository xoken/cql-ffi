#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "cassandra.h"
#include "bindings.h"

int insert_misc_store(){ // Maybe Bool, Int32, Maybe Int64, Text
  
  CassError rc = CASS_OK;
  CassStatement* statement = NULL;
  CassTuple* item = NULL;
  CassSession* sess = getSession();
  printf("%p",sess);
  const char* query = "INSERT INTO xoken.misc_store (key, value) VALUES (?, ?);";
  CassFuture* future;
  statement = cass_statement_new(query, 2);

  item = cass_tuple_new(2);
  cass_tuple_set_bool(item, 0,cass_true);
  cass_tuple_set_int32(item, 1, (cass_int32_t)5);
  cass_tuple_set_int64(item, 2, (cass_int64_t)10);
  cass_tuple_set_string(item, 3, "tup4");

  cass_statement_bind_string(statement, 0, "key");
  cass_statement_bind_tuple(statement, 1, item);

  future = cass_session_execute(sess, statement);
  cass_future_wait(future);
  rc = cass_future_error_code(future);
  if (rc != CASS_OK) {
    print_error(future);
    cass_future_free(future);
    return -1;
  }

  cass_future_free(future);
  return 0;
}

int run(){
    return insert_misc_store("k",12,"v");
}