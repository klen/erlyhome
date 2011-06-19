{ application, randgen, [
        { description, "AboutEcho homework random generator."},
        { vsn, "0.1.0"},
        { modules, [
            randgen,
            randgen_app,
            randgen_sup,
            randgen_seed
        ]},
        { applications, [ kernel, stdlib ]},
        { registered, [ randgen_sup ]},
        { mod, { randgen_app, [] }},
        { env, [

            % Max number of random generators.
            { num_gen, 10 },

            % URI
            { uri, "http://localhost:4000/" },

            % Max value
            { max_value, 100 }

        ]}
]}.
