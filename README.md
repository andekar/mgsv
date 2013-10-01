mgsv
====
Delete user debts
URL:  delete_user_debt
DATA: 
'''json
      [ {request_by : anders@gmail.com}
      , {uid        : robert.f@gmail.com}]
'''
RETURN: [{status : ok}] or [{error, request_failed}]
NOTE: this will remove the debt between the two given persons completely

curl -X PUT -H "Content-type: application/json" http://localhost:8000/payapp/delete_user_debt -d "[{\"request_by\":\"anders@gmail.com\"}, {\"uid\":\"robert.f@gmail.com\"}]"

Delete debt(s)
URL:  delete_debt
DATA: [ {request_by : anders@gmail.com}
      , {uuid       : 61c1e712-6f4c-4deb-8c9c-bb4276d65f07}
	]
RETURN: [{status : ok}] or [{error, request_failed}]
NOTE: this can contain several uuid to different persons

curl -X PUT -H "Content-type: application/json" http://localhost:8000/payapp/delete_debt -d "[{\"request_by\":\"jenny@gmail.com\"},{\"uuid\":\"61c1e712-6f4c-4deb-8c9c-bb4276d65f07\"}]"

[
    {
        "request_by": "jenny@gmail.com"
    },
    {
        "uuid": "61c1e712-6f4c-4deb-8c9c-bb4276d65f07"
    }
]

return "ok"

Register user(s)
URL:  register
DATA: [ {request_by : anders@gmail.com}
      , {uid        : robert.f@gmail.com}
      , {name       : Robert}
      , {usertype   : gmail | local | facebook}
      , {currency   : SEK | NOK | ...}
      ]
RETURN: // TODO
-- register user(s)
curl -X PUT -H "Content-type: application/json" http://localhost:8000/payapp/register -d "[{\"uid\":\"robert.f@gmail.com\"},{\"name\":\"Robert\"}, {\"usertype\":\"gmail\"}, {\"currency\":\"SEK\"}]"
[
    {
        "uid": "robert.f@gmail.com"
    },
    {
        "name": "Robert"
    }
]

Lookup user(s)
URL:  users
DATA: [ {request_by : anders@gmail.com}
      , {uid        : jenny@gmail.com}
      , ...
      ]
RETURN: [ { uid       : anders@gmail.com
	  , user      : anders
	  , user_type : gmail | local | facebook
	  , currency  : SEK | NOK | ...}
	, ...
	] or [{error, request_failed}]
-- lookup user(s)
curl -X PUT -H "Content-type: application/json" http://localhost:8000/payapp/users -d "[{\"uid\":\"anders@gmail.com\"},{\"uid\":\"jenny_karlsson@live.com\"}]"
[
    {
        "uid": "anders@gmail.com"
    },
    {
        "uid": "jenny_karlsson@live.com"
    }
]

response
[
    {
        "uid": "anders@gmail.com",
        "user": "Anders",
        "user_type": "gmail|local"
    },
    {
        "uid": "jenny@gmail.com",
        "user": "jenny",
        "user_type": "gmail|local"
    }
]

Transfer debts
URL:  transfer_debts
DATA: [ {request_by : anders@gmail.com}
      ,	{old_uid    : 01190f79-c0f2-45f9-a2bd-b37d19b85337}
      , {new_uid    : jenny@gmail.com}
      ]
RESPONSE: [{status : ok}] or [{error, request_failed}]
NOTE: this transfers debts between request_by and old_uuid to request_by and new_uuid where new_uuid must already exist

curl -X PUT -H "Content-type: application/json" http://localhost:8000/payapp/transfer_debts -d "[{\"request_by\":\"anders@gmail.com\"},{\"old_uid\":\"01190f79-c0f2-45f9-a2bd-b37d19b85337\"},{\"new_uid\":\"jenny@gmail.com\"}]"

[
    {
        "request_by": "anders@gmail.com"
    },
    {
        "old_uid": "01190f79-c0f2-45f9-a2bd-b37d19b85337"
    },
    {
        "new_uid": "jenny@gmail.com"
    }
]

-- PUT
curl -X PUT -H "Content-type: application/json" http://localhost:8000/payapp -d "[{\"debt\":{\"user1\":\"Anders\",\"uid1\":\"andersk84@gmail.com\",\"reason\":\"bio\",\"amount\":100,\"user2\":\"Petter\",\"misc\":{\"test\":\"val\"}}},{\"request_by\":\"andersk84@gmail.com\"}]"
[
    {
        "debt": {
            "user1": "Anders",
            "uid1": "anders@gmail.com",
            "reason": "bio",
            "amount": 100,
            "user2": "Petter",
            "misc": {
                "test": "val"
            }
        }
    },
    {
        "request_by": "anders@gmail.com"
    }
]

OPTIONALS:
uid1, uid2, user1, user2, timestamp, misc

returns:

[
    {
        "debt": {
            "uid1": "anders@gmail.com",
            "user1": "Anders Karlsson",
            "uid2": "ce9a6a0e-14de-4108-a6e7-5f966607758c",
            "user2": "Petter",
            "uuid": "4c81a051-d4c7-49b1-b09d-d580dd4b7dcf",
            "reason": "bio",
            "amount": 100,
            "timestamp": 1368119368067262,
            "status": "ok",
            "misc": {
                "test": "val"
            }
        }
    }
]

GET

curl http://localhost:8000/payapp/users
[
    {
        "uid": "jenny@gmail.com",
        "user": "Jenny Karlsson",
        "usertype": "gmail"
    },
    {
        "uid": "7e72bfc3-fb49-4380-8182-91662b19c22e",
        "user": "Kalle",
        "usertype": "local"
    }
]


curl http://localhost:8000/payapp/user_transactions/anders@gmail.com
[
    {
        "debt": {
            "uuid": "cc0deda9-f9ba-4655-83af-d7501b20a247",
            "uid1": "andersk84@gmail.com",
            "uid2": "b783b74c-a231-48d5-a7b8-25d7f9af2e0f",
            "timestamp": 1368123585174608,
            "reason": "bio",
            "amount": 100,
            "misc": []
        }
    },
    {
        "debt": {
            "uuid": "1725a382-9c6a-4b17-b27f-7ea9cd995994",
            "uid1": "andersk84@gmail.com",
            "uid2": "robert.f@gmail.com",
            "timestamp": 1368123585231514,
            "reason": "bio",
            "amount": 100,
            "misc": []
        }
    }
]

curl http://localhost:8000/payapp/user_debt/anders@gmail.com
[
    {
        "debt": {
            "uid1": "andersk84@gmail.com",
            "uid2": "be526383-ba35-4110-a716-f6be520240e9",
            "amount": -100
        }
    },
    {
        "debt": {
            "uid1": "andersk84@gmail.com",
            "uid2": "c7082a27-affb-4fb6-812a-104db3e319f6",
            "amount": -100
        }
    }
]


================ Change notes ==================
------ V0.2g changes
- fix transfer debts so that it does not add debts several times

------ V0.2f changes
- fix transfer debts which was broken
- Add possibility to change username in server

------ V0.2e changes
- removed approve debts
- add functionaliy to remove debts (same way as removing what previously was approval debts)


------ V0.2d changes


------ V0.2c changes

----------- api



