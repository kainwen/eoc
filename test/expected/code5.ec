{cvar_prog,[{{label,start},
             {cvar_seq,[{cvar_assign,'tmp.0',{cvar_neg,{cvar_int,10}}},
                        {cvar_assign,'x.1',
                                     {cvar_plus,{cvar_int,42},{cvar_var,'tmp.0'}}}],
                       {cvar_return,{cvar_plus,{cvar_var,'x.1'},{cvar_int,10}}}}}]}.
