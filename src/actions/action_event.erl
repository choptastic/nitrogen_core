% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (action_event).
-include("wf.hrl").
-export([
        render_action/1,
        maybe_wire_next/2
    ]).

render_action(#event { 
        postback=Postback,
        actions=Actions,
        anchor=Anchor,
        trigger=Trigger,
        target=Target,
        validation_group=ValidationGroup,
        handle_invalid=HandleInvalid,
        on_invalid=OnInvalid,
        type=Type,
        keycode=KeyCode,
        shift_key=ShiftKey,
        delay=Delay,
        delegate=Delegate, 
        extra_param=ExtraParam
    }) -> 

    ValidationGroup1 = wf:coalesce([ValidationGroup, Trigger]),
    AnchorScript = wf_render_actions:generate_anchor_script(Anchor, Target), 
    PostbackScript = generate_postback_script(Type, Postback, Anchor, ValidationGroup1, HandleInvalid, OnInvalid, Delegate, ExtraParam),
    {EffectiveType, EffectiveKeyCode} = effective_type_and_keycode(Type, KeyCode),
    WireAction = #wire { trigger=Trigger, target=Target, actions=Actions },

    Script = case Type of

        %% SYSTEM EVENTS %%%
        % Trigger a system postback immediately...
        system when Delay == 0 ->
            [
                AnchorScript, PostbackScript, WireAction
            ];

        % Trigger a system postback after some delay...
        system ->
            TempID = wf:to_binary(wf:temp_id()),
            [
                <<AnchorScript/binary, "document.",TempID/binary,"  = function() {">>,
                    PostbackScript, WireAction,
                <<"}; setTimeout(\"document.",TempID/binary,"(); document.",TempID,"=null;\", ",(wf:to_binary(Delay))/binary,");">>
            ];

        %% USER EVENTS %%%

        % Handle keypress, keydown, or keyup when a keycode is defined...
        _ when ((EffectiveType==keypress orelse EffectiveType==keydown orelse EffectiveType==keyup) andalso (EffectiveKeyCode /= undefined)) ->
            [
                <<"Nitrogen.$observe_event('~s', '~s', '~s', function(event) {">>, [Anchor, Trigger, EffectiveType]),
                    wf:f(<<"if (Nitrogen.$is_key_code(event, ~p, ~p)) { ">>, [EffectiveKeyCode, ShiftKey]),
                        AnchorScript, PostbackScript, WireAction, 
                        %wf:f("alert('~p:~p');",[EffectiveType, EffectiveKeyCode]),
                    <<"return false;">>,
                <<"}});">>
            ];

        % Run the event after a specified amount of time
        timer ->
            TempID = wf:temp_id(),
            [
                wf:f(<<"document.~s = function() {">>, [TempID]), 
                    AnchorScript, PostbackScript, WireAction, 
                <<"};">>,
                wf:f(<<"setTimeout(\"document.~s(); document.~s=null;\", ~p);">>, [TempID, TempID, Delay])
            ];

        default ->
            [
                AnchorScript, PostbackScript, WireAction
            ];

        % Run some other Javascript event (click, mouseover, mouseout, etc.)
        _ when Delay == 0 ->
            [
                wf:f(<<"Nitrogen.$observe_event('~s', '~s', '~s', function(event) {">>, [Anchor, Trigger, Type]), 
                AnchorScript, PostbackScript, WireAction, 
                <<"});">>
            ];
        _ ->
            [
                wf:f(<<"Nitrogen.$observe_event('~s', '~s', '~s', function(event) {setTimeout(function(){ ">>, [Anchor, Trigger, Type]), 
                    AnchorScript, PostbackScript, WireAction, 
                    wf:f(<<"}, ~p)">>, [Delay]),
                <<"});">>
            ]
    end,
    Script.


generate_postback_script(system, Postback, Anchor, ValidationGroup1, HandleInvalid, _OnInvalid, Delegate, _ExtraParam) ->
    wf_event:generate_system_postback_script(Postback, Anchor, ValidationGroup1, HandleInvalid, Delegate);
generate_postback_script(_, Postback, Anchor, ValidationGroup1, HandleInvalid, OnInvalid, Delegate, ExtraParam) ->
    wf_event:generate_postback_script(Postback, Anchor, ValidationGroup1, HandleInvalid, OnInvalid, Delegate, ExtraParam).

%% Simple conversion of convenience events enterkey and tabkey
effective_type_and_keycode(enterkey, _) -> {keydown, 13};
effective_type_and_keycode(tabkey, _) -> {keydown, 9};
effective_type_and_keycode(Type, KeyCode) -> {Type, KeyCode}.

maybe_wire_next(_Anchor, undefined) -> do_nothing;
maybe_wire_next(Anchor, Next) ->
    Next1 = wf_render_actions:normalize_path(Next),
    wf:defer(Anchor, #event{ type=tabkey, actions=wf:f(<<"Nitrogen.$go_next('~s');">>, [Next1])}).
