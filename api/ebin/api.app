{ application, api, [
        { description, "AboutEcho homework application."},
        { vsn, "0.1.0"},
        { modules, [
            api,
            api_app,
            api_sup,
            api_server,
            json2
        ]},
        {applications, [ kernel, stdlib ]},
        {registered, [ api_sup ]},
        {mod, { api_app, [] }},
        {env, [

            % Max number of saved sequences
            { num_seq, 10 }

        ]}
]}.

