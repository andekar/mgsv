mgsv
====

# HTTPS

**NOTE** in the https request the Authorization field of the https header must contain {usertype}:{userid}:{usertoken} coded in Base64 as defined in the standard.

**URL:**  debts/{id}
##
**METHOD** DELETE
######
**DATA:** {id} user id of the other part
######
**RETURN:**
######
204 if successful
500 if not

**NOTE:** this will remove the debt between the two given persons completely

**URL:**  debts
##
**METHOD** PUT
######
**DATA:**
######
```json
[ {"request_by" : "anders@gmail.com"}
, {"old_uid"    : "01190f79-c0f2-45f9-a2bd-b37d19b85337"}
, {"new_uid"    : "jenny@gmail.com"}
]
```
**RESPONSE:**
######
```json
[{"status" : "ok"}]
```
**or**
```json
[{"error" : "request_failed"}]
```
**NOTE:** this transfers debts between request_by and old_uuid to request_by and new_uuid where new_uuid must already exist

**URL:**  debts
##
**METHOD** GET
######
**RESPONSE:**
######
```json
[ { "debt" : { "uid1"     : "anders@gmail.com"
             , "uid2"     : "petter@gmail.com"
             , "amount"   : 127
             , "currency" : "SEK" | "NOK" | ...
             }
  }
  , ...
]
**or**
```json
[{"error" : "request_failed"}]
```

**URL:**  transactions/{id}
##
**METHOD** DELETE
######
**DATA:** {id} id of the transaction
######
**RETURN:**
######
204 if successful
500 if not

**URL:**  transactions
##
**METHOD** POST
######
**DATA:**
######
```json
[ {"transaction" :
            { "user1"     : "anders"            // This one is optional if uid1 is given
            , "uid1"      : "anders@gmail.com"  // This one is optional if user1 is given
            , "reason"    : "cinema ticket Riddick"
            , "amount"    : 100
            , "user2"     : "petter"            // This one is optional if uid2 is given
            , "uid2"      : "petter@gmail.com"  // This one is optional if user2 is given
            , "currency"  : "SEK" | "NOK" | ...
            , "timestamp" : 12346567            // seconds since 1970
            , "echo_uuid" : "324237483"         // any client created guid
            , "org_debt"  : { "exchange_rate"   : 0.18  // this is in case there is a second currency involved
                            , "currency" : "SEK" | "NOK" | ...
                            }
            }
  }
, ... // more transactions
]
```
**RESPONSE:**
######
```json
[ {"transaction" :
           { "uuid"       : "uniqueid"
           , "user1"      : "anders"
           , "uid1"       : "anders@gmail.com"  // This will be a created one if not given at input
           , "user_type1" : "gmail" | "local" | "facebook"
           , "reason"     : "cinema ticket Riddick"
           , "amount"     : 100
           , "user2"      : "petter"
           , "uid2"       : "petter@gmail.com"  // This will be a created one if not given at input
           , "user_type2" : "gmail" | "local" | "facebook"
           , "currency"   : "SEK" | "NOK" | ...
           , "timestamp"  : 12346567            // seconds since 1970
           , "server_timestamp" : 12346567     // seconds since 1970
           , "echo_uuid"  : "324237483"         // any client created guid
           , "status"     : "ok" | "failed"
           , "org_debt"  : { "exchange_rate"   : 0.18  // this is in case there is a second currency involved
                           , "currency" : "SEK" | "NOK" | ...
                           }
           }
  }
, ... // more transactions
]
```
**or**
```json
[{"error" : "request_failed"}]
```

**URL:**  transactions/[num] | [from]/[num] | [userid]/[from]/[num]
##
**METHOD** GET
######
**DATA:**
######
[userid] - id of other party to find transactions between
[num]  - number of transactions to return
[from] - begin at another location in the sorted list of transactions
**RESPONSE:**
######
```json
[ {"transaction" :
           { "uuid"       : "uniqueid"
           , "uid1"       : "anders@gmail.com"  // This will be a created one if not given at input
           , "usertype1"  : "gmail" | "local" | "facebook"
           , "reason"     : "cinema ticket Riddick"
           , "amount"     : 100
           , "uid2"       : "petter@gmail.com"  // This will be a created one if not given at input
           , "usertype2"  : "gmail" | "local" | "facebook"
           , "currency"   : "SEK" | "NOK" | ...
           , "timestamp"  : 12346567            // seconds since 1970
           , "server_timestamp" : 12346567     // seconds since 1970
           , "org_debt"   : { "exchange_rate"   : 0.18  // this is in case there is a second currency involved
                           , "currency" : "SEK" | "NOK" | ...
                           }
           }
  }
, ... // more transactions
]
```
**NOTE** the list returned is sorted based on SERVER_TIMESTAMP

**URL:**  users
##
**METHOD** PUT
######
**DATA:**
######
```json
[ {"user" : "anders"}
| {"currency" : "SEK" | "NOK" | ...}
]
**RETURN:**
######
```json
[{"status" : "ok"}]
```
**or**
```json
[{"error" : "request_failed"}]
```

**URL:**  users
##
**METHOD** POST
######
**DATA:**
######
```json
[ {user : {"uid"        : "robert.f@gmail.com"} // OPTIONAL if not given, one will be created
        , {"name"       : "Robert"}
        , {"usertype"   : "gmail" | "local" | "facebook"} // OPTIONAL if not given it will become either gmail or local
        , {"currency"   : "SEK" | "NOK" | ...}
        , {"echo_uuid"  : "a_unique_id"}
  }
]
```
**RETURN:**
######
```json
[ {user : {"uid"              : "robert.f@gmail.com"}
        , {"name"             : "Robert"}
        , {"usertype"         : "gmail" | "local" | "facebook"}
        , {"currency"         : "SEK" | "NOK" | ...}
        , {"server_timestamp" : time_since_1970}
        , {"echo_uuid"  : "a_unique_id"}
  }
]
```
**or**
```json
[{"error" : "request_failed"}]
```

**URL:**  users/{uid}/{uid}/...
##
**METHOD** GET
######
**DATA:** {uid}/{uid}/... - user id:s of users to lookup
######
**RETURN:**
######
```json
[ {user : { "uid"       : "anders@gmail.com"
          , "user"      : "anders"
          , "user_type" : "gmail" | "local" | "facebook"
          , "currency"  : "SEK" | "NOK" | ...
          , "server_timestamp" : time_since_1970 }
  }
, ...
]
```
**or**
```json
[{"error" : "request_failed"}]
```

**URL:**  feedback/{id}
##
**METHOD** DELETE
######
**DATA:** {id} of feedback to remove
######
**RETURN:**
######
204 if successful
500 if not

**URL:**  feedback
##
**METHOD** POST
######
**DATA:**
######
```json
[ {"feedback" : "whatever"}
]
```
**RETURN:**
######
```json
{ "feedback" :
  { "uid"        : "b6ac0146-9d46-4277-a4fa-7bc8fd320b60"
  , "request_by" : "anders@gmail.com"
  , "feedback"   : "whatever"
  , "server_timestamp":1381214519568941 }
}
```
**or**
```json
[{"error" : "request_failed"}]
```

**URL:**  feedback
##
**METHOD** GET
######
**RETURN:**
######
```json
[{ "feedback" :
  { "uid"        : "b6ac0146-9d46-4277-a4fa-7bc8fd320b60"
  , "request_by" : "anders@gmail.com"
  , "feedback"   : "whatever"
  , "server_timestamp":1381214519568941 }
 }, ...
]
```