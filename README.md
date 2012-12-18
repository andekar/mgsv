mgsv
====

------ V0.2f changes
- fix transfer debts which was broken
- Add possibility to change username in server

------ V0.2e changes
- removed approve debts
- add functionaliy to remove debts (same way as removing what previously was approval debts)


------ V0.2d changes


------ V0.2c changes

-- both of the users can remove the debt

curl -X PUT -H "Content-type: application/json" http://localhost:8000/payapp/delete_debt -d "[{\"request_by\":\"jenny@gmail.com\"},{\"uuid\":\"61c1e712-6f4c-4deb-8c9c-bb4276d65f07\"}]"

-- register users
curl -X PUT -H "Content-type: application/json" http://localhost:8000/payapp/register -d "[{\"uid\":\"robert.f@gmail.com\"},{\"name\":\"Robert\"}]"

-- this will show the same for both users even if one of them has approved the debt

curl http://localhost:8000/payapp/not_approved_user_transactions/anders@gmail.com
[{"debt":{"uuid":"61c1e712-6f4c-4deb-8c9c-bb4276d65f07","uid1":"anders@gmail.com","uid2":"jenny@gmail.com","timestamp":1351371359532248,"reason":"bio","amount":100}}]

-- user constraint, the one approving the debt must be another one than the one who added the debt.

curl -X PUT -H "Content-type: application/json" http://localhost:8000/payapp/approve_debt -d "[{\"request_by\":\"jenny@gmail.com\"},{\"uuid\":\"e4aa232e-97c2-4b30-bc6e-97f65d97dfd8\"}]"

curl -X PUT -H "Content-type: application/json" http://localhost:8000/payapp/users -d "[{\"uid\":\"anders@gmail.com\"},{\"uid\":\"jenny_karlsson@live.com\"}]"

response

[{"uid":"anders@gmail.com","user":"Anders"},{"uid":"jenny@gmail.com","user":"jenny"}]

curl -X PUT -H "Content-type: application/json" http://localhost:8000/payapp/transfer_debts -d "[{\"request_by\":\"anders@gmail.com\"},{\"old_uid\":\"01190f79-c0f2-45f9-a2bd-b37d19b85337\"},{\"new_uid\":\"jenny@gmail.com\"}]"

------ V0.2b
Requests:
PUT

curl -X PUT -H "Content-type: application/json" http://localhost:8000/payapp -d "[{\"debt\":{\"user1\":\"Anders\",\"uid1\":\"anders@gmail.com\",\"reason\":\"bio\",\"amount\":100,\"user2\":\"Petter\"}}]"

OPTIONALS:
uid1, uid2, user1, user2, timestamp

returns:

[{"debt":{"uid1":"anders@gmail.com","user1":"Anders","uid2":"c28e645f-fc07-4d92-a68a-098c34b59c51","user2":"Petter","uuid":"e5400f88-cd5c-40e3-9b1c-2914739ad415","reason":"bio","amount":100,"timestamp":9759138,"status":"ok"}}]

GET

curl http://localhost:8000/payapp/users
[{"uid":"anders@gmail.com","user":"Anders"},{"uid":"f364e184-4b94-4b51-948c-32c7f041a70c","user":"Petter"},{"uid":"bf902d38-9c86-4b7a-89f7-260392743fca","user":"Mattias"}]

curl http://localhost:8000/payapp/user_debt/anders@gmail.com
[{"debt":{"uid1":"anders@gmail.com","uid2":"bf902d38-9c86-4b7a-89f7-260392743fca","amount":-175}},{"debt":{"uid1":"anders@gmail.com","uid2":"f364e184-4b94-4b51-948c-32c7f041a70c","amount":-309}}]

curl http://localhost:8000/payapp/debts
[{"debt":{"user1":"anders@gmail.com","user2":"f364e184-4b94-4b51-948c-32c7f041a70c","amount":-309}},{"debt":{"user1":"anders@gmail.com","user2":"bf902d38-9c86-4b7a-89f7-260392743fca","amount":-175}},{"debt":{"user1":"bf902d38-9c86-4b7a-89f7-260392743fca","user2":"bf902d38-9c86-4b7a-89f7-260392743fca","amount":-175}},{"debt":{"user1":"bf902d38-9c86-4b7a-89f7-260392743fca","user2":"f364e184-4b94-4b51-948c-32c7f041a70c","amount":-950}}]

curl http://localhost:8000/payapp/user_transactions/anders@gmail.com
[{"debt":{"uuid":"d646a86d-ee44-41d7-a63b-2474c28750ed","uid1":"anders@gmail.com","uid2":"bf902d38-9c86-4b7a-89f7-260392743fca","timestamp":9687972,"reason":"Burgare","amount":-175}},{"debt":{"uuid":"333677cd-e6e1-4ab5-a2d0-0fce424ea3ca","uid1":"anders@gmail.com","uid2":"f364e184-4b94-4b51-948c-32c7f041a70c","timestamp":9694298,"reason":"Dykning","amount":-234}},{"debt":{"uuid":"a05b5c31-7bf3-499a-9d5a-6ee31fc6cb95","uid1":"anders@gmail.com","uid2":"f364e184-4b94-4b51-948c-32c7f041a70c","timestamp":9689915,"reason":"bio","amount":100}},{"debt":{"uuid":"c5ef3ebb-5b57-4de1-b3d7-6fd013c8196c","uid1":"anders@gmail.com","uid2":"f364e184-4b94-4b51-948c-32c7f041a70c","timestamp":9691834,"reason":"Burgare","amount":-175}}]

curl http://localhost:8000/payapp/user_debt/anders@gmail.com
[{"debt":{"uid1":"anders@gmail.com","uid2":"bf902d38-9c86-4b7a-89f7-260392743fca","amount":-175}},{"debt":{"uid1":"anders@gmail.com","uid2":"f364e184-4b94-4b51-948c-32c7f041a70c","amount":-309}}]

curl http://localhost:8000/payapp/transactions
[{"debt":{"uuid":"c5ef3ebb-5b57-4de1-b3d7-6fd013c8196c","uid1":"anders@gmail.com","uid2":"f364e184-4b94-4b51-948c-32c7f041a70c","timestamp":9691834,"reason":"Burgare","amount":-175}},{"debt":{"uuid":"a05b5c31-7bf3-499a-9d5a-6ee31fc6cb95","uid1":"anders@gmail.com","uid2":"f364e184-4b94-4b51-948c-32c7f041a70c","timestamp":9689915,"reason":"bio","amount":100}},{"debt":{"uuid":"6c5fb6be-8557-45f1-b839-46efe0e50243","uid1":"bf902d38-9c86-4b7a-89f7-260392743fca","uid2":"bf902d38-9c86-4b7a-89f7-260392743fca","timestamp":9685846,"reason":"Burgare","amount":-175}},{"debt":{"uuid":"333677cd-e6e1-4ab5-a2d0-0fce424ea3ca","uid1":"anders@gmail.com","uid2":"f364e184-4b94-4b51-948c-32c7f041a70c","timestamp":9694298,"reason":"Dykning","amount":-234}},{"debt":{"uuid":"d646a86d-ee44-41d7-a63b-2474c28750ed","uid1":"anders@gmail.com","uid2":"bf902d38-9c86-4b7a-89f7-260392743fca","timestamp":9687972,"reason":"Burgare","amount":-175}},{"debt":{"uuid":"a29629e4-fcba-4ff0-b294-e07a081aa6a0","uid1":"bf902d38-9c86-4b7a-89f7-260392743fca","uid2":"f364e184-4b94-4b51-948c-32c7f041a70c","timestamp":9666711,"reason":"Cykel","amount":-950}}]
