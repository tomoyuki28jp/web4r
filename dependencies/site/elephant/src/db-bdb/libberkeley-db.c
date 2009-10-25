/*
;;;
;;; libsleepycat.c -- C wrappers for Sleepycat for FFI
;;; 
;;; Initial version 8/26/2004 by Ben Lee
;;; <blee@common-lisp.net>
;;; 
;;; part of
;;;
;;; Elephant: an object-oriented database for Common Lisp
;;;
;;; Copyright (c) 2004 by Andrew Blumberg and Ben Lee
;;; <ablumberg@common-lisp.net> <blee@common-lisp.net>
;;;
;;; Portions Copyright (c) 2005-2007 by Robert Read and Ian Eslick
;;; <rread common-lisp net> <ieslick common-lisp net>
;;;
;;; Elephant users are granted the rights to distribute and use this software
;;; as governed by the terms of the Lisp Lesser GNU Public License
;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;
*/

#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <wchar.h>

/* Some utility stuff used to be here but has been placed in
   libmemutil.c  */

/* Pointer arithmetic utility functions */
/* should these be in network-byte order? probably not..... */
/* Pointer arithmetic utility functions */
/* should these be in network-byte order? probably not..... */
int read_int(unsigned char *buf, int offset) {
  int i;
  memcpy(&i, buf+offset, sizeof(int));
  return i;
}

int read_uint(unsigned char *buf, int offset) {
  unsigned int ui;
  memcpy(&ui, buf+offset, sizeof(unsigned int));
  return ui;
}

int32_t read_int32(unsigned char *buf, int offset) {
  int32_t i;
  memcpy(&i, buf+offset, sizeof(int32_t));
  return i;
}

uint32_t read_uint32(unsigned char *buf, int offset) {
  uint32_t ui;
  memcpy(&ui, buf+offset, sizeof(uint32_t));
  return ui;
}

int64_t read_int64(unsigned char *buf, int offset) {
  int64_t i;
  memcpy(&i, buf+offset, sizeof(int64_t));
  return i;
}

uint64_t read_uint64(unsigned char *buf, int offset) {
  uint64_t ui;
  memcpy(&ui, buf+offset, sizeof(uint64_t));
  return ui;
}

float read_float(unsigned char *buf, int offset) {
  float f;
  memcpy(&f, buf+offset, sizeof(float));
  return f;
}

double read_double(unsigned char *buf, int offset) {
  double d;
  memcpy(&d, buf+offset, sizeof(double));
  return d;
}

/* Platform specific integer */
void write_int(unsigned char *buf, int num, int offset) {
  memcpy(buf+offset, &num, sizeof(int));
}

void write_uint(unsigned char *buf, unsigned int num, int offset) {
  memcpy(buf+offset, &num, sizeof(unsigned int));
}


/* Well-defined integer widths */
void write_int32(unsigned char *buf, int32_t num, int offset) {
  memcpy(buf+offset, &num, sizeof(int32_t));
}

void write_uint32(unsigned char *buf, uint32_t num, int offset) {
  memcpy(buf+offset, &num, sizeof(uint32_t));
}

void write_int64(unsigned char *buf, int64_t num, int offset) {
  memcpy(buf+offset, &num, sizeof(int64_t));
}

void write_uint64(unsigned char *buf, uint64_t num, int offset) {
  memcpy(buf+offset, &num, sizeof(uint64_t));
}

void write_float(unsigned char *buf, float num, int offset) {
  memcpy(buf+offset, &num, sizeof(float));
}

void write_double(unsigned char *buf, double num, int offset) {
  memcpy(buf+offset, &num, sizeof(double));
}

unsigned char *offset_charp(unsigned char *p, int offset) {
  return p + offset;
}

void copy_buf(unsigned char *dest, int dest_offset, unsigned char *src, int src_offset, 
	      int length) {
  memcpy(dest + dest_offset, src + src_offset, length);
}


/* Berkeley DB stuff */

#include <db.h>

/* Environment */

/* All "creation" functions return the create object, not the errno.
   This simplifies FFI handling. */

/* These next two functions are also needed because in db42 these
   are #define macros */
DB_ENV *db_env_cr(u_int32_t flags, int *errno) {
  DB_ENV *envp;
  *errno = db_env_create(&envp, flags);
  return envp;
}

char *db_strerr(int error) {
  return db_strerror(error);
}

int db_env_close(DB_ENV *env, u_int32_t flags) {
  return env->close(env, flags);
}

int db_env_open(DB_ENV *env, char *home, u_int32_t flags, int mode) {
  return env->open(env, home, flags, mode);
}

int db_env_dbremove(DB_ENV *env, DB_TXN *txnid, char *file, char *database,
		    u_int32_t flags) {
  return env->dbremove(env, txnid, file, database, flags);
}

int db_env_dbrename(DB_ENV *env, DB_TXN *txnid, char *file, char *database,
		    char *newname, u_int32_t flags) {
  return env->dbrename(env, txnid, file, database, newname, flags);
}

int db_env_remove(DB_ENV *env, char *home, u_int32_t flags) {
  return env->remove(env, home, flags);
}

int db_env_set_flags(DB_ENV *dbenv, u_int32_t flags, int onoff) {
  return dbenv->set_flags(dbenv, flags, onoff);
}

int db_env_get_flags(DB_ENV *dbenv, u_int32_t *flagsp) {
  return dbenv->get_flags(dbenv, flagsp);
}

int db_env_txn_checkpoint(DB_ENV *dbenv, u_int32_t kbyte, u_int32_t min,
			  u_int32_t flags) {
  return dbenv->txn_checkpoint(dbenv, kbyte, min, flags);
}


/* Database */

DB *db_cr(DB_ENV *dbenv, u_int32_t flags, int *errno) {
  DB *dbp;
  *errno = db_create(&dbp, dbenv, flags);
  return dbp;
}

int db_close(DB *db, u_int32_t flags) {
  return db->close(db, flags);
}

int db_open(DB *db, DB_TXN *txnid, char *file, char *database, DBTYPE type,
	    u_int32_t flags, int mode) {
  return db->open(db, txnid, file, database, type, flags, mode);
}

int db_remove(DB *db, char *file, char *database, u_int32_t flags) {
  return db->remove(db, file, database, flags);
}

int db_rename(DB *db, char *file, char *database, char *newname,
	      u_int32_t flags) {
  return db->rename(db, file, database, newname, flags);
}

int db_sync(DB *db, u_int32_t flags) {
  return db->sync(db, flags);
}

int db_truncate(DB *db, DB_TXN *txnid, u_int32_t *countp, u_int32_t flags) {
  return db->truncate(db, txnid, countp, flags);
}

int db_set_flags(DB *db, u_int32_t flags) {
  return db->set_flags(db, flags);
}

int db_get_flags(DB *db, u_int32_t *flagsp) {
  return db->get_flags(db, flagsp);
}

int db_set_pagesize(DB *db, u_int32_t pagesize) {
  return db->set_pagesize(db, pagesize);
}

int db_get_pagesize(DB *db, u_int32_t *pagesizep) {
  return db->get_pagesize(db, pagesizep);
}

void db_set_error_file(DB *db, char *filename) {
  return db->set_errfile(db, fopen(filename, "w+"));
}

/* Accessors */
/* We manage our own buffers (DB_DBT_USERMEM). */

int db_get_raw(DB *db, DB_TXN *txnid, 
	       unsigned char *key, u_int32_t key_size,
	       unsigned char *buffer, u_int32_t buffer_length,
	       u_int32_t flags, u_int32_t *result_size) {
  DBT DBTKey, DBTValue;
  int ret;
  
  memset(&DBTKey, 0, sizeof(DBT));
  memset(&DBTValue, 0, sizeof(DBT));
  DBTKey.data = key;
  DBTKey.size = key_size;
  DBTValue.data = buffer;
  DBTValue.ulen = buffer_length;
  DBTValue.flags |= DB_DBT_USERMEM;
  
  ret = db->get(db, txnid, &DBTKey, &DBTValue, flags);
  *result_size = DBTValue.size;
  
  return ret;
}

int db_put_raw(DB *db, DB_TXN *txnid, 
	       unsigned char *key, u_int32_t key_size,
	       unsigned char *value, u_int32_t value_size,
	       u_int32_t flags) {
  DBT DBTKey, DBTValue;
  
  memset(&DBTKey, 0, sizeof(DBT));
  memset(&DBTValue, 0, sizeof(DBT));
  DBTKey.data = key;
  DBTKey.size = key_size;
  DBTValue.data = value;
  DBTValue.size = value_size;
  
  return db->put(db, txnid, &DBTKey, &DBTValue, flags);
}

int db_del(DB *db, DB_TXN *txnid, 
	   unsigned char *key, u_int32_t key_size,
	   u_int32_t flags) {
  DBT DBTKey;
  
  memset(&DBTKey, 0, sizeof(DBT));
  DBTKey.data = key;
  DBTKey.size = key_size;
  return db->del(db, txnid, &DBTKey, flags);
}

int db_compact(DB *db, DB_TXN *txnid, 
	       unsigned char *start, u_int32_t start_size,
	       unsigned char *stop, u_int32_t stop_size,
	       u_int32_t flags,
	       unsigned char *end, u_int32_t end_length,
	       u_int32_t *end_size) {
  DBT DBTStart, DBTStop, DBTEnd;
  int errno;
  
  memset(&DBTStart, 0, sizeof(DBT));
  DBTStart.data = start;
  DBTStart.size = start_size;

  memset(&DBTStop, 0, sizeof(DBT));
  DBTStop.data = stop;
  DBTStop.size = stop_size;

  memset(&DBTEnd, 0, sizeof(DBT));
  DBTEnd.data = end;
  DBTEnd.ulen = end_length;
  DBTEnd.flags |= DB_DBT_USERMEM;

  errno = db->compact(db, txnid, 
		     &DBTStart,
		     &DBTStop,
		     NULL,
		     flags,
		      &DBTEnd);
  *end_size = DBTEnd.size;

  return errno;
}
		     
  

/* Cursors */

DBC * db_cursor(DB *db, DB_TXN *txnid, u_int32_t flags, int *errno) {
  DBC *cursor;
  *errno = db->cursor(db, txnid, &cursor, flags);
  return cursor;
}

int db_cursor_close(DBC *cursor) {
  return cursor->c_close(cursor);
}

int db_cursor_del(DBC *cursor, u_int32_t flags) {
  return cursor->c_del(cursor, flags);
}

DBC * db_cursor_dup(DBC *cursor, u_int32_t flags, int *errno) {
  DBC *dup;
  *errno = cursor->c_dup(cursor, &dup, flags);
  return dup;
}

int db_cursor_get_raw(DBC *cursor, 
		      unsigned char *keybuf, u_int32_t keybuf_size,
		      u_int32_t keybuf_length,
		      unsigned char *buffer, u_int32_t buffer_size,
		      u_int32_t buffer_length,
		      u_int32_t flags, u_int32_t *ret_key_size,
		      u_int32_t *result_size) {
  DBT DBTKey, DBTValue;
  int ret;
  
  memset(&DBTKey, 0, sizeof(DBT));
  memset(&DBTValue, 0, sizeof(DBT));
  DBTKey.data = keybuf;
  DBTKey.size = keybuf_size;
  DBTKey.ulen = keybuf_length;
  DBTKey.flags |= DB_DBT_USERMEM;
  DBTValue.data = buffer;
  DBTValue.size = buffer_size;
  DBTValue.ulen = buffer_length;
  DBTValue.flags |= DB_DBT_USERMEM;
  
  ret = cursor->c_get(cursor, &DBTKey, &DBTValue, flags);
  *ret_key_size = DBTKey.size;
  *result_size = DBTValue.size;
  
  return ret;
}

int db_cursor_pget_raw(DBC *cursor, 
		       unsigned char *keybuf, u_int32_t keybuf_size,
		       u_int32_t keybuf_length,
		       unsigned char *pkeybuf, u_int32_t pkeybuf_size,
		       u_int32_t pkeybuf_length,
		       unsigned char *buffer, u_int32_t buffer_size,
		       u_int32_t buffer_length,
		       u_int32_t flags, 
		       u_int32_t *ret_key_size,
		       u_int32_t *ret_pkey_size,
		       u_int32_t *result_size) {
  DBT DBTKey, DBTPKey, DBTValue;
  int ret;
  
  memset(&DBTKey, 0, sizeof(DBT));
  memset(&DBTPKey, 0, sizeof(DBT));
  memset(&DBTValue, 0, sizeof(DBT));
  DBTKey.data = keybuf;
  DBTKey.size = keybuf_size;
  DBTKey.ulen = keybuf_length;
  DBTKey.flags |= DB_DBT_USERMEM;
  DBTPKey.data = pkeybuf;
  DBTPKey.size = pkeybuf_size;
  DBTPKey.ulen = pkeybuf_length;
  DBTPKey.flags |= DB_DBT_USERMEM;
  DBTValue.data = buffer;
  DBTValue.size = buffer_size;
  DBTValue.ulen = buffer_length;
  DBTValue.flags |= DB_DBT_USERMEM;
  
  ret = cursor->c_pget(cursor, &DBTKey, &DBTPKey, &DBTValue, flags);
  *ret_key_size = DBTKey.size;
  *ret_pkey_size = DBTPKey.size;
  *result_size = DBTValue.size;
  
  return ret;
}

int db_cursor_put_raw(DBC *cursor,
		      unsigned char *key, u_int32_t key_size,
		      unsigned char *value, u_int32_t value_size,
		      u_int32_t flags) {
  DBT DBTKey, DBTValue;
  
  memset(&DBTKey, 0, sizeof(DBT));
  memset(&DBTValue, 0, sizeof(DBT));
  DBTKey.data = key;
  DBTKey.size = key_size;
  DBTValue.data = value;
  DBTValue.size = value_size;
  
  return cursor->c_put(cursor, &DBTKey, &DBTValue, flags);
}


/* Silently does nothing if the key/value isn't found.
   Can't use auto-commit here! */
int db_del_kv(DB *db, DB_TXN *tid, 
	      unsigned char *key, u_int32_t key_size,
	      unsigned char *value, u_int32_t value_size) {
  DBT DBTKey, DBTValue;
  DBC *cursor;
  int ret, c_ret;

  memset(&DBTKey, 0, sizeof(DBT));
  DBTKey.data = key;
  DBTKey.size = key_size;
  memset(&DBTValue, 0, sizeof(DBT));
  DBTValue.data = value;
  DBTValue.size = value_size;
  
  if ((ret = db->cursor(db, tid, &cursor, 0)) != 0)
    return ret;

  if ((ret = cursor->c_get(cursor, &DBTKey, &DBTValue, DB_GET_BOTH)) != 0)
    goto fail;

  ret = cursor->c_del(cursor, 0);

 fail:
  if ((c_ret = cursor->c_close(cursor)) != 0)
    return c_ret;
  return ret;
}

/* Bulk retrieval */

int db_cursor_get_multiple_key(DBC *cursor, 
			       unsigned char *keybuf, u_int32_t keybuf_size,
			       u_int32_t keybuf_length,
			       unsigned char *buffer, u_int32_t buffer_size,
			       u_int32_t buffer_length,
			       u_int32_t flags, u_int32_t *ret_key_size,
			       u_int32_t *result_size,
			       void **pointer, DBT **data) {
  DBT DBTKey, DBTValue;
  int ret;
  
  memset(&DBTKey, 0, sizeof(DBT));
  memset(&DBTValue, 0, sizeof(DBT));
  DBTKey.data = keybuf;
  DBTKey.size = keybuf_size;
  DBTKey.ulen = keybuf_length;
  DBTKey.flags |= DB_DBT_USERMEM;
  DBTValue.data = buffer;
  DBTValue.size = buffer_size;
  DBTValue.ulen = buffer_length;
  DBTValue.flags |= DB_DBT_USERMEM;
  
  flags |= DB_MULTIPLE_KEY;
  ret = cursor->c_get(cursor, &DBTKey, &DBTValue, flags);
  *ret_key_size = DBTKey.size;
  *result_size = DBTValue.size;
  if ((DBTKey.size <= DBTKey.ulen) && (DBTValue.size <= DBTValue.ulen)) {
    **data = DBTValue;
    DB_MULTIPLE_INIT(*pointer, *data);
  }
				    
  return ret;
}

void db_multiple_key_next(void *pointer, DBT *data,
			  unsigned char **key, u_int32_t *ret_key_size,
			  unsigned char **result, u_int32_t *result_size) {
  DB_MULTIPLE_KEY_NEXT(pointer, data,
		       *key, *ret_key_size,
		       *result, *result_size);
}

/* Transactions */

DB_TXN *db_txn_begin(DB_ENV *env, DB_TXN *parent, 
		      u_int32_t flags, int *errno) {
  DB_TXN * p;
  *errno = env->txn_begin(env, parent, &p, flags);
  return p;
}

int db_txn_abort(DB_TXN *txnid) {
  return txnid->abort(txnid);
}

int db_txn_commit(DB_TXN *txnid, u_int32_t flags) {
  return txnid->commit(txnid, flags);
}


int db_txnp_begin(DB_ENV *env, DB_TXN *parent, DB_TXN **txnp,
		 u_int32_t flags) {
  return env->txn_begin(env, parent, txnp, flags);
}

/* Sequences */

DB_SEQUENCE * db_sequence_create2(DB *db, u_int32_t flags, int *errno) {
  DB_SEQUENCE * seq;
  *errno = db_sequence_create(&seq, db, flags);
  return seq;
}

int db_sequence_open(DB_SEQUENCE *seq, DB_TXN *txnid, 
		     unsigned char *key, u_int32_t key_size, u_int32_t flags) {
  DBT DBTKey;
  memset(&DBTKey, 0, sizeof(DBT));
  DBTKey.data = key;
  DBTKey.size = key_size;
  
  return seq->open(seq, txnid, &DBTKey, flags);
}

int db_sequence_close(DB_SEQUENCE *seq, u_int32_t flags) {
  return seq->close(seq, flags);
}

/* db_seq_t = int64_t */
const unsigned int bitmask_32bits = 0xFFFFFFFF;
#define lower_u32bits(int64) ((unsigned int) int64 & bitmask_32bits)
#define upper_u32bits(int64) ((unsigned int) (int64 >> 32))
#define UUto64(low, high) ((((u_int64_t)high) << 32) | (u_int64_t)low)
#define lower_32bits(int64) ((int) int64 & bitmask_32bits)
#define upper_32bits(int64) ((int) (int64 >> 32))
#define USto64(low, high) ((((int64_t)high) << 32) | (u_int64_t)low)
  
int db_sequence_get(DB_SEQUENCE *seq, DB_TXN *txnid, int32_t delta,
		    u_int32_t *lowp, int32_t *highp, u_int32_t flags) {
  db_seq_t next;
  int ret;

  ret = seq->get(seq, txnid, delta, &next, flags);
  *lowp = lower_u32bits(next);
  *highp = upper_32bits(next);
  return ret;
}

int db_sequence_get_lower(DB_SEQUENCE *seq, DB_TXN *txnid, int32_t delta,
			  int32_t *lowp, u_int32_t flags) {
  db_seq_t next;
  int ret;

  ret = seq->get(seq, txnid, delta, &next, flags);
  *lowp = (int)lower_32bits(next);
  return ret;
}

/* Typo in the BDB docs! */
int db_sequence_initial_value(DB_SEQUENCE *seq, u_int32_t low, 
			      int32_t high) {
  return seq->initial_value(seq, USto64(low, high));
}

int db_sequence_remove(DB_SEQUENCE *seq, DB_TXN *txnid, u_int32_t flags) {
  return seq->remove(seq, txnid, flags);
}

int db_sequence_set_cachesize(DB_SEQUENCE *seq, int32_t size) {
  return seq->set_cachesize(seq, size);
}

int db_sequence_get_cachesize(DB_SEQUENCE *seq, int32_t *sizep) {
  return seq->get_cachesize(seq, sizep);
}

int db_sequence_set_flags(DB_SEQUENCE *seq, u_int32_t flags) {
  return seq->set_flags(seq, flags);
}

int db_sequence_set_range(DB_SEQUENCE *seq, u_int32_t minlow, 
			  int32_t minhigh, u_int32_t maxlow, 
			  int32_t maxhigh) {
  return seq->set_range(seq, USto64(minlow, minhigh), USto64(maxlow, maxhigh));
}

int db_sequence_get_range(DB_SEQUENCE *seq, u_int32_t *minlowp, 
			  int32_t *minhighp, u_int32_t *maxlowp, 
			  int32_t *maxhighp) {
  int64_t min, max;
  int errno;
  errno = seq->get_range(seq, &min, &max);
  *minlowp = lower_u32bits(min);
  *minhighp = upper_32bits(min);
  *maxlowp = lower_u32bits(max);
  *maxhighp = upper_32bits(max);
  return errno;
}

/* Locks and timeouts */

u_int32_t db_txn_id(DB_TXN *tid) {
  return tid->id(tid);
}

int db_env_lock_id(DB_ENV *env, u_int32_t *idp) {
  return env->lock_id(env, idp);
}

int db_env_lock_id_free(DB_ENV *env, u_int32_t id) {
  return env->lock_id_free(env, id);
}

int db_env_lock_get(DB_ENV *env, u_int32_t locker,
		    u_int32_t flags, unsigned char *object, u_int32_t object_size,
		    const db_lockmode_t lock_mode, DB_LOCK *lock) {
  DBT DBTObject;
  memset(&DBTObject, 0, sizeof(DBT));
  DBTObject.data = object;
  DBTObject.size = object_size;

  return env->lock_get(env, locker, flags, &DBTObject, lock_mode, lock);
}

int db_env_lock_put(DB_ENV *env, DB_LOCK *lock) {
  return env->lock_put(env, lock);
}

int db_env_lock_vec(DB_ENV *env, u_int32_t locker, u_int32_t flags,
		    DB_LOCKREQ list[], int nlist, DB_LOCKREQ **elistp) {
  return env->lock_vec(env, locker, flags, list, nlist, elistp);
}

/* db_timeout_t = u_int32_t */
int db_env_set_timeout(DB_ENV *env, db_timeout_t timeout, u_int32_t flags) {
  return env->set_timeout(env, timeout, flags);
}

int db_env_get_timeout(DB_ENV *env, db_timeout_t *timeoutp, u_int32_t flags) {
  return env->get_timeout(env, timeoutp, flags);
}

int db_env_set_cachesize(DB_ENV *env, u_int32_t gbytes, u_int32_t bytes, int ncache) {
  return env->set_cachesize(env, gbytes, bytes, ncache);
}

int db_env_get_cachesize(DB_ENV *env, u_int32_t *gbytes, u_int32_t *bytes, int *ncache) {
  return env->get_cachesize(env, gbytes, bytes, ncache);
}

int db_env_set_lk_detect(DB_ENV *env, u_int32_t detect) {
    return env->set_lk_detect(env, detect);
}

int db_env_get_lk_detect(DB_ENV *env, u_int32_t *detectp) {
    return env->get_lk_detect(env, detectp);
}

int db_env_lock_detect(DB_ENV *env, u_int32_t flags, u_int32_t atype,
		   int *aborted) {
    return env->lock_detect(env, flags, atype, aborted);
}

/* Secondary indices */

int db_associate(DB *primary, DB_TXN *txnid, DB *secondary,
		 int (*callback)(DB *, const DBT *, const DBT *, DBT *),
		 u_int32_t flags) {
  return primary->associate(primary, txnid, secondary, callback, flags);
}

int never_index(DB *db, const DBT *key, const DBT *data, DBT *result) {
  return DB_DONOTINDEX;
}

int db_fake_associate(DB *primary, DB_TXN *txnid, DB *secondary, 
		      u_int32_t flags) {
  return primary->associate(primary, txnid, secondary, &never_index, flags);
}

/* Poor man's counters */

int next_counter(DB_ENV *env, DB *db, DB_TXN *parent,
		 unsigned char *key, u_int32_t key_size,
		 unsigned char *lockid, u_int32_t lockid_size) {
  DB_LOCK lock;
  DBT DBTKey, DBTData; 
  DB_TXN *tid; 
  int counter, tries, ret, t_ret, lockheld;
  u_int32_t id;

  /* Initialization. */ 
  memset(&lock, 0, sizeof(lock)); 
  memset(&DBTKey, 0, sizeof(DBTKey)); 
  memset(&DBTData, 0, sizeof(DBTData)); 
  DBTKey.data = key;
  DBTKey.size = key_size;
  DBTData.data = lockid;
  DBTData.size = lockid_size;

  tries = 0;

 loop:
  lockheld = 0;

  /* Begin the transaction. */ 
  if ((ret = env->txn_begin(env, parent, &tid, 0)) != 0) { 
    env->err(env, ret, "DB_ENV->txn_begin"); 
    return (-1); 
  }

  id = tid->id(tid);

  if ((ret = env->lock_get(env, id, 0, &DBTData, DB_LOCK_WRITE, &lock)) != 0)
    goto fail;

  lockheld = 1;

  memset(&DBTData, 0, sizeof(DBTData)); 
  DBTData.data = &counter;
  DBTData.ulen = sizeof(counter);
  DBTData.flags |= DB_DBT_USERMEM;

  if ((ret = db->get(db, tid, &DBTKey, &DBTData, 0)) != 0) 
    goto fail;

  ++counter;

  memset(&DBTData, 0, sizeof(DBTData)); 
  DBTData.data = &counter;
  DBTData.size = sizeof(counter);

  if ((ret = db->put(db, tid, &DBTKey, &DBTData, 0)) != 0) 
    goto fail;

  if ((ret = env->lock_put(env, &lock)) != 0) 
    goto fail;

  if ((ret = tid->commit(tid, DB_TXN_NOSYNC)) != 0) { 
    env->err(env, ret, "DB_TXN->commit"); 
    return (-2); 
  } 
  return (counter); 


 fail:
  if (lockheld)
    if ((ret = env->lock_put(env, &lock)) != 0) 
      return (-3);

  /* Abort and retry the operation. */ 
  if ((t_ret = tid->abort(tid)) != 0) { 
    env->err(env, t_ret, "DB_TXN->abort"); 
    return (-4);
  } 
  if (tries++ == 100)
    return (-5); 
  goto loop;
}


/* BDB KEY ORDER DEFINITIONS */

int db_set_bt_compare(DB *db,
		      int (*bt_compare_fcn)(DB *db, const DBT *dbt1, 
					    const DBT *dbt2)) {
  return db->set_bt_compare(db, bt_compare_fcn);
}

int db_set_dup_compare(DB *db,
		       int (*dup_compare_fcn)(DB *db, const DBT *dbt1, 
					      const DBT *dbt2)) {
  return db->set_dup_compare(db, dup_compare_fcn);
}

#include <math.h>

#define S_RESERVED 0xF0

double read_num(unsigned char *buf);
int case_cmp(const unsigned char *a, int32_t length1, const unsigned char *b, int32_t length2);
int wcs_cmp(const wchar_t *a, int32_t length1, const wchar_t *b, int32_t length2);
int lex_cmp(const unsigned char *a, int32_t length1, const unsigned char *b, int32_t length2);
int utf16_cmp(const unsigned char *s1, int32_t length1, 
	      const unsigned char *s2, int32_t length2);

#define S1_FIXNUM 1
#define S1_CHAR 2
#define S1_SINGLE_FLOAT 3
#define S1_DOUBLE_FLOAT 4
#define S1_NEGATIVE_BIGNUM 5
#define S1_POSITIVE_BIGNUM 6
#define S1_RATIONAL 7

#define S1_NIL 8

#define S1_UCS1_SYMBOL 9
#define S1_UCS1_STRING 10
#define S1_UCS1_PATHNAME 11
#define S1_UCS2_SYMBOL 12
#define S1_UCS2_STRING 13
#define S1_UCS2_PATHNAME 14
#define S1_UCS4_SYMBOL 20
#define S1_UCS4_STRING 21
#define S1_UCS4_PATHNAME 22

#define S1_PERSISTENT 15
#define S1_CONS 16
#define S1_HASH_TABLE 17
#define S1_OBJECT 18
#define S1_ARRAY 19

#define S1_FILL_POINTER_P 0x40
#define S1_ADJUSTABLE_P 0x80

/* Inspired by the BDB docs.  We have to memcpy to
   insure memory alignment. */

#define type_numeric1(c) ((c)<8)

/* Original serializer */
int lisp_compare1(DB *dbp, const DBT *a, const DBT *b) {
  int difference;
  double ddifference;
  unsigned char *ad, *bd, at, bt;
  ad = (unsigned char *)a->data;
  bd = (unsigned char *)b->data;

  /* Compare OIDs. */
  difference = read_int(ad, 0) - read_int(bd, 0);
  if (difference) return difference;
  
  /* Have a type tag? */
  if (a->size == 4) 
    if (b->size == 4)
      return 0;
    else
      return -1;
  else if (b->size == 4) 
    return 1;

  at = ad[4]; bt = bd[4];

  /* Compare numerics. */
  if (type_numeric1(at) && type_numeric1(bt)) {
    ddifference = read_num(ad+4) - read_num(bd+4);
    if (ddifference > 0) return 1;
    else if (ddifference < 0) return -1;
    return 0;
  }

  /* Compare types. */
  /* ISE: need extra conditional here...forget why, so research it */
  difference = at - bt;
  if (difference) return difference;

  /* Same type! */
  switch (at) {
  case S1_NIL: /* nil */
    return 0;
  case S_RESERVED:
    return ad[5] < bd[5]; /* different tags */
  case S1_UCS1_SYMBOL: /* 8-bit symbol */
  case S1_UCS1_STRING: /* 8-bit string */
  case S1_UCS1_PATHNAME: /* 8-bit pathname */
    return case_cmp(ad+9, read_int(ad, 5), bd+9, read_int(bd, 5));
  case S1_UCS2_SYMBOL:  /* 16-bit symbol */
  case S1_UCS2_STRING: /* 16-bit string */
  case S1_UCS2_PATHNAME: /* 16-bit pathname */
    return utf16_cmp(ad+9, read_int(ad, 5), bd+9, read_int(bd, 5));
  case S1_UCS4_SYMBOL:
  case S1_UCS4_STRING:
  case S1_UCS4_PATHNAME:
    return wcs_cmp((wchar_t*)ad+9, read_int(ad, 5), (wchar_t*)bd+9, read_int(bd, 5)); 
  default:
    return lex_cmp(ad+5, (a->size)-5, bd+5, (b->size)-5);
  }
}

#ifndef exp2
#define exp2(c) (pow(2,(c)))
#endif

double read_num(unsigned char *buf) {
  unsigned char *limit;
  double i, result, denom;
  switch (buf[0]) {
  case S1_FIXNUM:
    return (double)read_int(buf, 1);
  case S1_SINGLE_FLOAT:
    return (double)read_float(buf, 1);
  case S1_DOUBLE_FLOAT:
    return read_double(buf, 1);
  case S1_NEGATIVE_BIGNUM:
    result = 0;
    buf += 5;
    limit = buf + read_uint(buf, -4);
    for(i=0 ; buf < limit; i++, buf = buf+4) {
      result -= exp2(i*32) * read_uint(buf, 0);
    }
    return result;
  case S1_POSITIVE_BIGNUM:
    result = 0;
    buf += 5;
    limit = buf + read_uint(buf, -4);
    for(i=0 ; buf < limit; i++, buf = buf+4) {
      result += exp2(i*32) * read_uint(buf, 0);
    }
    return result;
  case S1_RATIONAL:
  default:
    switch ((++buf)[0]) {
    case S1_FIXNUM:
      result = (double)read_int(++buf, 0);
      buf += 4;
      break;
    case S1_NEGATIVE_BIGNUM:
      result = 0;
      buf += 5;
      limit = buf + read_uint(buf, -4);
      for(i=0 ; buf < limit; i++, buf = buf+4) {
	result -= exp2(i*32) - read_uint(buf, 0);
      }
      break;
    case S1_POSITIVE_BIGNUM:
    default:
      result = 0;
      buf += 5;
      limit = buf + read_uint(buf, -4);
      for(i=0 ; buf < limit; i++, buf = buf+4) {
	result += exp2(i*32) * read_uint(buf, 0);
      }
      break;
    }
    
    switch (buf[0]) {
    case S1_FIXNUM: 
      return result / read_int(++buf, 0);
    case S1_NEGATIVE_BIGNUM:
      denom = 0;
      buf += 5;
      limit = buf + read_uint(buf, -4);
      for(i=0 ; buf < limit; i++, buf = buf+4) {
	denom -= exp2(i*32) * read_uint(buf, 0);
      }
      return result / denom;
    case S1_POSITIVE_BIGNUM:
    default:
      denom = 0;
      buf += 5;
      limit = buf + read_uint(buf, -4);
      for(i=0 ; buf < limit; i++, buf = buf+4) {
	denom += exp2(i*32) * read_uint(buf, 0);
      }
      return result / denom;
    }    
  }
}

/*****************************
 SERIALIZER 2
******************************/

#define S2_FIXNUM32 1
#define S2_FIXNUM64 2
#define S2_CHAR 3
#define S2_SHORT_FLOAT 30
#define S2_SINGLE_FLOAT 4
#define S2_DOUBLE_FLOAT 5
#define S2_NEGATIVE_BIGNUM 6
#define S2_POSITIVE_BIGNUM 7
#define S2_RATIONAL 8
#define S2_UTF8_STRING 9
#define S2_UTF16_STRING 10
#define S2_UTF32_STRING 11
#define S2_PATHNAME 12
#define S2_SYMBOL 13
#define S2_SYMBOL_ID 14
#define S2_PERSISTENT 15
#define S2_CONS 16
#define S2_HASH_TABLE 17
#define S2_OBJECT 18
#define S2_ARRAY 19
#define S2_STRUCT 20
#define S2_CLASS 21
#define S2_COMPLEX 22
#define S2_NIL 0x3F

#define S2_FILL_POINTER_P 0x40
#define S2_ADJUSTABLE_P 0x80

#define type_numeric2(c) (((c)<9) || ((c)==22))

/******
  Serialized BTree keys have the form:
  BTree OID + tag(s) + [values]

  Slot values have the form:
  SVal OID + tag(s) + [values]
 
*****/  

double read_num2(unsigned char *buf);

/* New serializer */
int lisp_compare2(DB *dbp, const DBT *a, const DBT *b) {
  int difference;
  int offset;
  double ddifference;
  unsigned char *ad, *bd, at, bt;
  ad = (unsigned char *)a->data;
  bd = (unsigned char *)b->data;

  /* Compare OIDs: OIDs are limited by native integer width */
  difference = read_int(ad, 0) - read_int(bd, 0);
  if (difference) return difference;
  
  /* Have a type tag? */
  if (a->size == 4)
    if (b->size == 4)
      return 0;
    else
      return -1;
  else if (b->size == 4) 
    return 1;

  /* Get the 8-bit tag */
  at = ad[4]; bt = bd[4];

  /******
  printf("Tag1: %d Tag2: %d\n", bd[4], bd[4]);
  printf("Tag1b: %d Tag2b: %d\n", bd[5], bd[5]);
  *******/

  /* Compare numerics. */
  if ((type_numeric2(at)) && (type_numeric2(bt))) {
    ddifference = read_num2(ad+4) - read_num2(bd+4);
    if (ddifference > 0) return 1;
    else if (ddifference < 0) return -1;
    return 0;
  }

  /* Compare types. */
  difference = at - bt;
  if (difference) return difference;

  /* Otherwise at == bt, so types are same */

  /* Handle prefix types */
  switch(at) {
  case S2_SYMBOL:
  case S2_PATHNAME:
    /* Make sure the strings are both of the same radix */
    difference = ad[5] - bd[5];
    if (difference) return difference;
    offset = 1;
  default:
    offset = 0;
  }

    
  /* Same type*/
  switch (at) {
  case S2_NIL: /* nil */
    return 0;
  case S_RESERVED:
    return ad[5] < bd[5]; /* different tags */
  case S2_UTF8_STRING: /* 8-bit string */
    return case_cmp(ad+9+offset, read_int32(ad+offset, 5), bd+9+offset, read_int32(bd+offset, 5));
  case S2_UTF16_STRING: /* 16-bit string */
    return utf16_cmp(ad+9+offset, read_int32(ad+offset, 5), bd+9+offset, read_int32(bd+offset, 5));
  case S2_UTF32_STRING:
    return wcs_cmp((wchar_t*)ad+9+offset, read_int32(ad+offset, 5), (wchar_t*)bd+9+offset, read_int32(bd+offset, 5)); 
  default:
    return lex_cmp(ad+5+offset, (a->size)-5, bd+5+offset, (b->size)-5);
  }
}

/* Support for multiple serializers.  Versions are integers starting at 1 */

int db_set_lisp_compare(DB *db, int version) {
  switch (version) {
  case 1: 
    return db->set_bt_compare(db, &lisp_compare1);
  default:
    return db->set_bt_compare(db, &lisp_compare2);
  }
}

int db_set_lisp_dup_compare(DB *db, int version) {
  switch (version) {
  case 1: 
    return db->set_dup_compare(db, &lisp_compare1);
  default:
    return db->set_dup_compare(db, &lisp_compare2);
  }
}

double read_num2(unsigned char *buf) {
  unsigned char *limit;
  double i, result, denom;
  switch (buf[0]) {
  case S2_FIXNUM32:
  case S2_FIXNUM64:
  case S2_SYMBOL_ID:
    return (double)read_int(buf, 1);
  case S2_SHORT_FLOAT:
    return (double)read_float(buf, 1);
  case S2_SINGLE_FLOAT:
    return (double)read_float(buf, 1);
  case S2_DOUBLE_FLOAT:
    return read_double(buf, 1);
  case S2_NEGATIVE_BIGNUM:
    result = 0;
    buf += 5;
    limit = buf + read_uint(buf, -4);
    for(i=0 ; buf < limit; i++, buf = buf+4) {
      result -= exp2(i*32) * read_uint(buf, 0);
    }
    return result;
  case S2_POSITIVE_BIGNUM:
    result = 0;
    buf += 5;
    limit = buf + read_uint(buf, -4);
    for(i=0 ; buf < limit; i++, buf = buf+4) {
      result += exp2(i*32) * read_uint(buf, 0);
    }
    return result;
  case S2_RATIONAL:
  default:
    switch ((++buf)[0]) {
    case S2_FIXNUM32:
    case S2_FIXNUM64:
      result = (double)read_int(++buf, 0);
      buf += 4;
      break;
    case S2_NEGATIVE_BIGNUM:
      result = 0;
      buf += 5;
      limit = buf + read_uint(buf, -4);
      for(i=0 ; buf < limit; i++, buf = buf+4) {
	result -= exp2(i*32) - read_uint(buf, 0);
      }
      break;
    case S2_POSITIVE_BIGNUM:
    default:
      result = 0;
      buf += 5;
      limit = buf + read_uint(buf, -4);
      for(i=0 ; buf < limit; i++, buf = buf+4) {
	result += exp2(i*32) * read_uint(buf, 0);
      }
      break;
    }
    
    switch (buf[0]) {
    case S2_FIXNUM32: 
    case S2_FIXNUM64:
      return result / read_int(++buf, 0);
    case S2_NEGATIVE_BIGNUM:
      denom = 0;
      buf += 5;
      limit = buf + read_uint(buf, -4);
      for(i=0 ; buf < limit; i++, buf = buf+4) {
	denom -= exp2(i*32) * read_uint(buf, 0);
      }
      return result / denom;
    case S2_POSITIVE_BIGNUM:
    default:
      denom = 0;
      buf += 5;
      limit = buf + read_uint(buf, -4);
      for(i=0 ; buf < limit; i++, buf = buf+4) {
	denom += exp2(i*32) * read_uint(buf, 0);
      }
      return result / denom;
    }    
  }
}

#ifdef WIN32
#define strncasecmp _strnicmp
typedef unsigned short uint16_t;
#endif

int case_cmp(const unsigned char *a, int32_t length1, const unsigned char *b, int32_t length2) {
  int min, sizediff, diff;
  sizediff = length1 - length2;
  min = sizediff > 0 ? length2 : length1;
  diff = strncasecmp((char *)a, (char *)b, min);
  if (diff == 0) return sizediff;
  return diff;
}

int wcs_cmp(const wchar_t *a, int32_t length1, 
	    const wchar_t *b, int32_t length2) {
  int min, sizediff, diff;
  sizediff = length1 - length2;
  min = sizediff > 0 ? length2 : length1;
  diff = wcsncmp(a, b, min /4);
  if (diff == 0) return sizediff;
  return diff;
}

int lex_cmp(const unsigned char *a, int32_t length1, const unsigned char *b, int32_t length2) {
  int min, sizediff, diff;
  sizediff = length1 - length2;
  min = sizediff > 0 ? length2 : length1;
  diff = memcmp(a, b, min);
  if (diff == 0) return sizediff;
  return diff;
}

/* The following is derived from
   http://oss.software.ibm.com/cvs/icu/~checkout~/icu/source/common/ustring.c 
*/
typedef uint16_t UChar;

#define UTF_IS_LEAD(c) (((c)&0xfffffc00)==0xd800)
#define UTF_IS_TRAIL(c) (((c)&0xfffffc00)==0xdc00)

/* compare UTF-16 strings */
/* memcmp/UnicodeString style, both length-specified */
/* don't assume byte-aligned! */
int utf16_cmp(const unsigned char *s1, int32_t length1, 
	      const unsigned char *s2, int32_t length2) {
  const unsigned char *start1, *start2, *limit1, *limit2;
  UChar c1, c2;
  int32_t lengthResult;

  if(length1<length2) {
    lengthResult=-1;
    limit1=s1+length1;
  } else if(length1==length2) {
    lengthResult=0;
    limit1=s1+length1;
  } else /* length1>length2 */ {
    lengthResult=1;
    limit1=s1+length2;
  }

  if(s1==s2) return lengthResult;

  start1=s1;
  start2=s2;

  for(;;) {
    if(s1==limit1) return lengthResult;

    memcpy(&c1, s1, sizeof(UChar));
    memcpy(&c2, s2, sizeof(UChar));
    if(c1!=c2) break;

    s1 = s1 + 2;
    s2 = s2 + 2;
  }

  limit1=start1+length1;
  limit2=start2+length2;

  if(c1>=0xd800 && c2>=0xd800) {
    if(c1>=0xe000) 
      c1-=0x800;
    else
      c1+=0x2000;
    
    if(c2>=0xe000)
      c2-=0x800;
    else
      c2+=0x2000;
      
    /* here's some newer code which i can't make work
    if((c1<=0xdbff && (s1+1)!=limit1 && UTF_IS_TRAIL(*(s1+1))) ||
       (UTF_IS_TRAIL(c1) && start1!=s1 && UTF_IS_LEAD(*(s1-1)))) {
    } else {
      c1-=0x2800;
    }

    if((c2<=0xdbff && (s2+1)!=limit2 && UTF_IS_TRAIL(*(s2+1))) ||
       (UTF_IS_TRAIL(c2) && start2!=s2 && UTF_IS_LEAD(*(s2-1)))) {
    } else {
      c2-=0x2800;
      }*/
  }

  return (int32_t)c1-(int32_t)c2;
}


