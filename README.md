# Shopping List
Simple Rest Application with Polysemy.
Only to understand basic concept of polysemy effect.

## CURLs
```sh
## run app
stack run -- --port=9000

## Add item to list
➜  ~ curl -X POST -d '{"name": "Milk", "quantity": 1, "completed": false}' -H 'Content-type: application/json' http://localhost:9000

{"name":"Milk","quantity":1,"completed":false}%

➜  ~ curl -X GET  http://localhost:9000 | jq

{
  "1": {
    "name": "Small Bread",
    "quantity": 1,
    "completed": false
  },
  "2": {
    "name": "Bottled Water",
    "quantity": 1,
    "completed": false
  },
  "3": {
    "name": "Milk",
    "quantity": 1,
    "completed": false
  }
}

➜  ~ curl -X GET  http://localhost:9000/2 | jq

{
  "name": "Bottled Water",
  "quantity": 1,
  "completed": false
}

➜  ~ curl -X GET  http://localhost:9000/2/toggle | jq

{
  "name": "Bottled Water",
  "quantity": 1,
  "completed": true
}
```
