-module(test).
-include("wf.hrl").
-export([main/0]).

-compile({parse_transform, wf_auto_attr}).

main() ->
    Body = #br{class=whatever, random_attr="12345"},
    case Body of
        #br{blah_test=Something} ->
            Something;
        _ ->
            true
    end.


%    X = rand:uniform(),
%
%    [
%        if
%            X==0.5 ->
%                wf:wire(#alert{text="Whatever"}),
%                Body;
%            true ->
%                [
%                    #br{class=whatever, random_attr="12345"}
%                ]
%        end,
%        case wf:page_module() of
%            test ->
%                #button{random_attr2=something};
%            _ ->
%                ""
%        end,
%        #panel{},
%        #panel{body=[
%            #span{text=something, jesse_attr=1}
%        ]}
%    ].
%
