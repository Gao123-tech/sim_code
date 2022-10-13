function    new = new_ext( old, ext )
% NEW_EXT   Adds or changes the extension on an existing filename string  
%
% This code starts looking for the '.' in the filename from the end of 
% the string and terminates the search if a '/' is encountered. This means
% you can use this to add extensions onto prefixes even if you have
% directory names in the path that contain '.'s
%
% The new_extension must also contain the '.'
%
% eg          filename = new_ext( filename, '.new');
% 
% Amos L Maggi
% Centre for Marine Sciecne and Technology
% Department of Applied Physics 
% Curtin University
%
% Revision 0        06 November 1997 ... ALM

TRUE  = 1;
FALSE = 0;

len_name = length( old  );
len_ext  = length( ext  );

if ( len_name == 0 )
  new = ext;
else

  finished = FALSE    ;
  i        = len_name ;

  while ( ~ finished ),

    if     ( old(i) == '.' )

      i_end    = i-1  ; 
      finished = TRUE ;

    elseif ( old(i) == '\' | i == 1)    % ie path boundary detected
                                        % or out of characters
      i_end    = len_name     ;
      finished = TRUE         ;

    end

  i = i-1;

  end

  new = old(1:i_end);

  for j = 1:len_ext ,

    new(i_end + j) = ext(j) ;

  end

end