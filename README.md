# TrialChain mock

## How to build

* `stack build` builds the project
* `stack build --test` builds and runs tests
* `stack exec trialchain-mock-exe` runs the application

## Testing requests

Let's send several requests to make sure it works as expected.
First of all we have to build and run our application via `stack build && stack exec trialchain-mock-exe`.
The command runs the server with initial random balances (which include 3 predefined addresses: `abcdef`, `10bcaf` and `abdffe0123`).
After server has been run, it greets with message like this:
```
Initial balances
{
  abcdef: 100
, 10bcaf: 200
, abdffe0123: 300
}
```
Where keys are addresses, and values are corresponding balances.

Let's send the first transaction, which transfers 50 coins from `abcdef` to `10bcaf`:

```
curl -X POST -H "Content-Type: application/json" \
-d '{"id": 1, "rawTx": "0300000000000000abcdef00000000030000000000000010bcaf320000000100000000000000aa0100000000000000bb"}' \
localhost:8080/api/broadcast
```

As response we have to receive:

`{"result":"f153076407d147fb7c78d39ccfd9564431672336e556bcd0f1acc7b6bed7bab5","id":1}`

where `f153076407d147fb7c78d39ccfd9564431672336e556bcd0f1acc7b6bed7bab5` is the transaction id.

Let's check that this transaction really exists now:

```
curl -X GET -H "Content-Type: application/json" \
-d '{"id": 1, "txId": "f153076407d147fb7c78d39ccfd9564431672336e556bcd0f1acc7b6bed7bab5"}' \
localhost:8080/api/get_tx
```

As response we have to receive:

`{"result":"0300000000000000abcdef00000000030000000000000010bcaf320000000100000000000000aa0100000000000000bb","id":1}`

where `result` is equal to `rawTx` from the first request.

Let's send one more transaction, 50 coins from `abcdef` to `abdffe0123`:

```
curl -X POST -H "Content-Type: application/json" \
-d '{"id": 2, "rawTx": "0300000000000000abcdef010000000500000000000000abdffe0123320000000100000000000000aa0100000000000000bb"}' \
localhost:8080/api/broadcast
```

Server response has to be:

`{"result":"3c0fbf1be1c6f7a75e5fdd5f2683a052c3e982cb96bac2ce84f20628f43d3576","id":2}`

Since this moment `abcdef` is out of money.

Let's request this transaction:

```
curl -X GET -H "Content-Type: application/json" \
-d '{"id": 1, "txId": "3c0fbf1be1c6f7a75e5fdd5f2683a052c3e982cb96bac2ce84f20628f43d3576"}' \
localhost:8080/api/get_tx
```

Expectedly, server response is:

`{"result":"0300000000000000abcdef010000000500000000000000abdffe0123320000000100000000000000aa0100000000000000bb","id":1}`

Let's try to request non-existing transaction:

```
curl -X GET -H "Content-Type: application/json" \
-d '{"id": 1, "txId": "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"}' \
localhost:8080/api/get_tx
```

Server's response:

`{"error":"Tx #aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa not found.","id":1}`

And as last request, let's try to transfer 1 coin from `abcdef` to `abdffe0123`:

```
curl -X POST -H "Content-Type: application/json" \
-d '{"id": 3, "rawTx": "0300000000000000abcdef020000000500000000000000abdffe0123010000000100000000000000aa0100000000000000bb"}' \
localhost:8080/api/broadcast
```

Server has to reject this transaction with message because `abcdef` is out ouf money:

`{"error":"Balance of abcdef is -0, what is less than required amount 1.","id":3}`