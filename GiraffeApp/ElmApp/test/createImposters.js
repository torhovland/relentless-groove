var imposter = require('./imposter'),
productPort = 3000;

function createImposter() {
return imposter({
        port: productPort,
        protocol: "http",
        name: "Relentless Groove Service",
        stubs: [{
                predicates: [{
                    equals: { method: "OPTIONS" }
                }],
                responses: [{
                    is: {
                        headers: {
                            "Access-Control-Allow-Origin": "*",
                            "Access-Control-Allow-Methods": "GET, POST, PUT, PATCH, DELETE",
                            "Access-Control-Allow-Headers": "Content-Type"
                        }
                    }
                }]
            },
            {
                predicates: [{
                    equals: { method: "POST" }
                }],
                responses: [{
                    is: {
                        statusCode: 200,
                        headers: {
                            "Access-Control-Allow-Origin": "*",
                            "Content-Type": "application/json"
                        },
                        body: { "status": "Activity saved OK." }
                    }
                }]
            }
        ]
    })
    .create();
}

createImposter();
console.log("Added Areas imposter to mountebank.");