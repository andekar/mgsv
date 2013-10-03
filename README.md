mgsv
====
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
**URL:** user_transactions/uuid  // This will later be changed to be a PUT operation
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
**URL:** user_debt/uuid  // This will later be changed to be a PUT operation
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
