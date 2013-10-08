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
            , "org_debt"  : { "amount"   : 127  // this is in case there is a second currency involved
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
           { "user1"      : "anders"
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
           , "status"     : "ok"
           , "org_debt"  : { "amount"   : 127  // this is in case there is a second currency involved
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

**URL:**  transactions/[num] | [from]/[num]
##
**METHOD** GET
######
**DATA:**
######
[num]  - number of transactions to return
[from] - begin at another location in the sorted list of transactions
**RESPONSE:**
######
```json
[ {"transaction" :
           { "user1"      : "anders"
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
           , "status"     : "ok"
           , "org_debt"  : { "amount"   : 127  // this is in case there is a second currency involved
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
**NOTE** the list returned is sorted based on SERVER_TIMESTAMP

**URL:**  users/{newusername}
##
**METHOD** PUT
######
**DATA:** {newusername} - the new username
######
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
[ {"user" : {"uid"        : "robert.f@gmail.com"}
          , {"name"       : "Robert"}
          , {"usertype"   : "gmail" | "local" | "facebook"}
          , {"currency"   : "SEK" | "NOK" | ...}
  }
  , ...
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
  }
  , ...
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

# HTTP

Delete user debts
------
**URL:**  delete_user_debt
######
**DATA:**
######
```json
[ {"request_by" : "anders@gmail.com"}
, {"uid"        : "robert.f@gmail.com"}]
```
**RETURN:**
######
```json
[{"status" : "ok"}]
```
**or**
```json
[{"error" : "request_failed"}]
```
**NOTE:** this will remove the debt between the two given persons completely

Delete debt(s)
------
**URL:**  delete_debt
######
**DATA:**
######
```json
[ {"request_by" : "anders@gmail.com"}
, {"uuid"       : "61c1e712-6f4c-4deb-8c9c-bb4276d65f07"}
]
```
**RETURN:**
######
```json
[{"status" : "ok"}]
```
**or**
```json
[{"error" : "request_failed"}]
```
**NOTE:** this can contain several uuid to different persons

Register user(s)
------
**URL:**  register
######
**DATA:**
######
```json
[ {"request_by" : "anders@gmail.com"}
, {"uid"        : "robert.f@gmail.com"}
, {"name"       : "Robert"}
, {"usertype"   : "gmail" | "local" | "facebook"}
, {"currency"   : "SEK" | "NOK" | ...}
]
```
**RETURN:** // TODO
######

Lookup user(s)
------
**URL:**  users
######
**DATA:**
######
```json
[ {"request_by" : "anders@gmail.com"}
, {"uid"        : "jenny@gmail.com"}
, ...
]
```
**RETURN:**
######
```json
[ { "uid"       : "jenny@gmail.com"
  , "user"      : "Jenny"
  , "user_type" : "gmail" | "local" | "facebook"
  , "currency"  : "SEK" | "NOK" | ...
  }
, ...
]
```
**or**
```json
[{"error" : "request_failed"}]
```

Transfer debts
------
**URL:**  transfer_debts
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

Add debts
------
**URL:** /
######
**DATA:**
######
```json
[ {"debt" :
            { "user1"     : "anders"            // This one is optional if uid1 is given
            , "uid1"      : "anders@gmail.com"  // This one is optional if user1 is given
            , "reason"    : "cinema ticket Riddick"
            , "amount"    : 100
            , "user2"     : "petter"            // This one is optional if uid2 is given
            , "uid2"      : "petter@gmail.com"  // This one is optional if user2 is given
            , "currency"  : "SEK" | "NOK" | ...
            , "timestamp" : 12346567            // seconds since 1970
            , "echo_uuid" : "324237483"         // any client created guid
            , "org_debt"  : { "amount"   : 127  // this is in case there is a second currency involved
                            , "currency" : "SEK" | "NOK" | ...
                            }
            }
  }
, {"request_by" : "anders@gmail.com"}
, ... // more debts
]
```
**RESPONSE:**
######
```json
[ {"debt" :
           { "user1"      : "anders"
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
           , "status"     : "ok"
           , "org_debt"  : { "amount"   : 127  // this is in case there is a second currency involved
                           , "currency" : "SEK" | "NOK" | ...
                           }
           }
  }
, {"request_by" : "anders@gmail.com"}
, ... // more debts
]
```
**or**
```json
[{"error" : "request_failed"}]
```

users /** NOTE THIS WILL BE REMOVED SOON **/
------
**URL:** users
######
**REQUEST_TYPE:** GET
######
**RESPONSE:**
######
```json
[ { "uid"       : "anders@gmail.com"
  , "user"      : "anders"
  , "user_type" : "gmail" | "local" | "facebook"
  , "currency"  : "SEK" | "NOK" | ...
  }
, ...
]
```
**or**
```json
[{"error" : "request_failed"}]
```

User transactions
------
**URL:** user_transactions/uuid
######
**REQUEST_TYPE:** GET
######
**RESPONSE:**
######
```json
[ {"debt" :
           { "user1"      : "anders"
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
           , "status"     : "ok"
           , "org_debt"  : { "amount"   : 127  // this is in case there is a second currency involved
                           , "currency" : "SEK" | "NOK" | ...
                           }
           }
  }
, {"request_by" : "anders@gmail.com"}
, ... // more debts
]
```
**or**
```json
[{"error" : "request_failed"}]
```

User debt
------
**URL:** user_debt/uuid
######
**REQUEST_TYPE:** GET
######
**RESPONSE:**
######
```json
[ { "debt" : { "uid1"     : "anders@gmail.com"
             , "uid2"     : "petter@gmail.com"
             , "amount"   : 127
             , "currency" : "SEK" | "NOK" | ...
             }
  , ...
  }
]
```
