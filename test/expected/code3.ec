{cvar_prog,[{{label,start},
             {cvar_seq,[{cvar_assign,'x.1',{cvar_int,32}},
                        {cvar_assign,'x.2',{cvar_int,10}},
                        {cvar_assign,'tmp.0',{cvar_var,'x.2'}}],
                       {cvar_return,{cvar_plus,{cvar_var,'tmp.0'},
                                               {cvar_var,'x.1'}}}}}]}.
