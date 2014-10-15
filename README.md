mgsv
====

#Protocol = 0.36
#HTTPS
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
[ {"old_uid"    : "01190f79-c0f2-45f9-a2bd-b37d19b85337"}
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
[
   {
      "debt":{
         "id":"76277994-fc3f-4d6e-8ef3-6a94321cb9a0",
         "uid1":"890043b5-e69a-4af7-94a7-af9cac5a36d2",
         "uid2":"8cd677d5-b0a3-4f9e-bf02-0bd3d47b0069",
         "amount":2320.9629389298207,
         "currency":"SEK",
         "edit_details":{
            "created_at":1412457221192685,
            "created_by":"890043b5-e69a-4af7-94a7-af9cac5a36d2",
            "last_change":1412523623242310,
            "last_changed_by":"890043b5-e69a-4af7-94a7-af9cac5a36d2"
         }
      }
   },...
]
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
[
   {
      "transaction":{
         "reason":"test",
         "amount":1128.642364393294,
         "org_debt":{
            "currency":"CHF",
            "amount":150
         },
         "paid_by":"890043b5-e69a-4af7-94a7-af9cac5a36d2",
         "paid_for":"8cd677d5-b0a3-4f9e-bf02-0bd3d47b0069",
         "currency":"SEK",
         "echo_uuid":"69CAFDBE-60E0-4AAA-8051-B06C10D711B5"
      }
   }, ...
]
```
**RESPONSE:**
######
```json
[
   {
      "transaction":{
         "transaction_id":"6ae01e69-85fe-4fc4-a7ca-9cd22c85ed76",
         "paid_by":"890043b5-e69a-4af7-94a7-af9cac5a36d2",
         "paid_for":"8cd677d5-b0a3-4f9e-bf02-0bd3d47b0069",
         "amount":1128.642364393294,
         "reason":"test",
         "timestamp":1412523623240014,
         "server_timestamp":1412523623240014,
         "currency":"SEK",
         "org_transaction":{
            "amount":150,
            "currency":"CHF"
         },
         "edit_details":{
            "created_at":1412523623240072,
            "created_by":"890043b5-e69a-4af7-94a7-af9cac5a36d2",
            "last_change":1412523623240072,
            "last_changed_by":"890043b5-e69a-4af7-94a7-af9cac5a36d2"
         },
         "echo_uuid":"69CAFDBE-60E0-4AAA-8051-B06C10D711B5",
         "status":"ok"
      }
   }, ... // more transactions
]
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
same as when you post a transaction
**NOTE** the list returned is sorted based on SERVER_TIMESTAMP

**URL:**  users
##
**METHOD** PUT
######
**DATA:**
######
```json
[ {"displayname" : "anders"}
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
[
   {
      "user":{
         "usertype":"local",
         "currency":"SEK",
         "echo_uuid":"E084016F-D827-4B02-8E1D-0A2C60C81E08",
         "displayname":"testuser"
      }
   }
]
```
OR
```json
[
   {
      "user":{
         "usertype":"gmail", %% or facebook
         "currency":"SEK",
         "echo_uuid":"2C60A8E0-BB28-4A77-B3E2-7EDC613124E9",
         "uid":"106778044490122886188", %% this is the google or facebook id
         "displayname":"Simon unnamed"
      }
   }
]
```
**RETURN:**
######
```json
[
   {
      "user":{
         "internal_uid":"34d0e441-1067-41d8-b1d0-9a3f6f90e272", %the "most" unique
         "uid":"9ab28f42-5ab4-4f6c-aab0-d748dd2578aa", %normally gmail uid or fb uid visible to others
         "username":"9ab28f42-5ab4-4f6c-aab0-d748dd2578aa", % normally google email or fb private id
         "user_type":"local",
         "displayname":"testuser",
         "currency":"SEK",
         "user_edit_details":{
            "created_at":1412509260134837,
            "created_by":"890043b5-e69a-4af7-94a7-af9cac5a36d2",
            "last_change":1412509260134837,
            "last_changed_by":"890043b5-e69a-4af7-94a7-af9cac5a36d2"
         },
         "echo_uuid":"E084016F-D827-4B02-8E1D-0A2C60C81E08"
      }
   }
]
```
**or**
```json
[
   {
      "user":{
         "internal_uid":"f2ff3f5c-7208-4e8e-8f0b-f5f54ac7c895",
         "uid":"106778044490122886188",
         "username":"106778044490122886188", %% this become email if this user uses the app
         "user_type":"gmail",
         "displayname":"Simon unnamed",
         "currency":"SEK",
         "user_edit_details":{
            "created_at":1412508430670655,
            "created_by":"890043b5-e69a-4af7-94a7-af9cac5a36d2",
            "last_change":1412508430670858,
            "last_changed_by":"890043b5-e69a-4af7-94a7-af9cac5a36d2"
         },
         "echo_uuid":"2C60A8E0-BB28-4A77-B3E2-7EDC613124E9"
      }
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
same as when you create a user
**or**
```json
[{"error" : "request_failed"}]

**URL:**  countries
##
**METHOD** GET
######
**RETURN:**
######
```json
{"BTN" : "Bhutanese Ngultrum",
 ...}
```

**URL:**  rates
##
**METHOD** GET
######
**RETURN:**
######
```json
{"GMD" : 38.09304,
 ... }
```












# Protocol version OLD:
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
            { "user1"     : "anders"            // This one should only be given if uid1 uid1 is missing
            , "uid1"      : "anders@gmail.com"  // This one should only be given if user1 is not
            , "reason"    : "cinema ticket Riddick"
            , "amount"    : 100
            , "user2"     : "petter"            // same as user1
            , "uid2"      : "petter@gmail.com"  // same as uid1
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

**URL:**  countries
##
**METHOD** GET
######
**RETURN:**
######
```json
{"BTN" : "Bhutanese Ngultrum",
 ...}
```

**URL:**  rates
##
**METHOD** GET
######
**RETURN:**
######
```json
{"GMD" : 38.09304,
 ... }
```