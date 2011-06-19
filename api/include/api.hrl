%% -------------------------------------------------------------------
%%  @author Kirill Klenov <horneds@gmail.com>
%%          
%% 
%%  @copyright 2011 horneds@gmail.com   
%% 
%%  @doc Api structures.
%%  @end                      
%% -------------------------------------------------------------------

-author('Kirill Klenov <horneds@gmail.com>').

-record( state, { seq=[], store=[], limit=10 } ).
