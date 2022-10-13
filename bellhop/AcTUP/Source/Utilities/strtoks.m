function [toks, n] = strtoks(str, delim)
%STRTOKS          Same as strtok except all tokens returned as cell vector
%
%USAGE   [toks, n] = strtoks(str, delim); 
%         toks     = strtoks(str, delim); 
%
%         n        = length(toks);
%
% Amos L Maggi
% Centre for Marine Sciecne and Technology
% Department of Applied Physics 
% Curtin University
%
% Revision 0.0  -  16 February 2006 ... ALM
toks = {}  ;
[tok, rem] = strtok(str, delim);   
n          = 0;
if ~isempty(tok)
   n       = n+1;
   toks{n} = tok;
   done    = 0  ;
elseif isempty(tok)
   done    = 1  ;
elseif isempty(rem)
   done = 1;
end
while ~done
   [tok, rem] = strtok(rem, delim);   
   if isempty(rem) 
      done = 1;
   end
   if ~isempty(tok)   % this is because (last) rem may only contain delims ... these are not tokens
      n       = n+1;
      toks{n} = tok;
   end
end
   
