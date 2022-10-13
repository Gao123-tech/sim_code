function    [t2, varargout] = fget_t2( num, which, filename, nhead )
%fget_t2    Gets a 2nd order matrix from file
%
%usage      t2               = fget_t2( num, which, filename )
%           t2               = fget_t2( num, which, filename, nhead )  
%           [t2, headlines]  = fget_t2( num, which, filename, nhead ) 
%
%             num       = number of columns or rows (must know at
%                         least one of these)
%
%             which     = 'row' or 'col' and tells the fn what num is.
%             
%             filename  = includes path
%
%             nhead     = number of header rows (lines) in text file
%
%             t2        = output data matrix
%
%             headlines = cell array of headers
%
%
% Australian Maritime Engineering CRC Ltd
%
% Revision 1.0  20 November 1997  ALM
%
% Revision 1.1  18 February 2003  ALM
%               - able to ignore header lines
%
% Revision 1.11 05 March 2003  ALM
%               - bug fix (now rets headlines)


t2 = [];
headlines = {};
which = lower(which);
if nargin <= 3
  nhead = 0;
end


if ( filename == 0 )
  fprintf( 1, 'No filename ... bye...\n' );
else

  % OPEN TF FILE
  in_file    = fopen(filename, 'rt');
  if ( in_file == -1 )
    fprintf( 1, 'Unable to open tensor file %s\n', filename );
  else
    % get rid of headers
    for  ii = 1:nhead        
      headlines{ii} = fgetl(in_file);
    end
    if ( strcmp(which, 'row') )
      n_row = num;
      % Get number of columns       
      in_vector = fscanf( in_file, '%f' )  ;
      frewind( in_file )                   ;
      n_col = length(in_vector) / n_row    ;
      clear in_vector                      ;  
      % get rid of headers - (again after rewind)
      for  ii = 1:nhead        
        headlines{ii} = fgetl(in_file);
      end
    else
      n_col = num;
    end
    t2 = fscanf( in_file, '%f', [n_col, Inf] );
    fclose( in_file );
    t2 = t2';
  end
end

if nargout == 2
  varargout{1} = headlines;
end
