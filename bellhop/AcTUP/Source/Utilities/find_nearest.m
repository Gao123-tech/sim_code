function  [nearest, index] = find_nearest(vector, value)
%find_nearest    Searches vector for nearest matching value
%                This is for a monotonically increasing or decreasing vector 
%                If value is mid way between two points the lower indexed
%                element is returned
%                
%usage  [nearest, index] = find_nearest(vector, value)
%
% Amos L Maggi
% Centre for Marine Sciecne and Technology
% Department of Applied Physics 
% Curtin University
%
% Revision 0     24 September 1999

if isempty(vector)
  nearest = []; 
  index   = [];
else
  len = length(vector);
  if     value <= vector(1)
    nearest = vector(1);
    index   = 1;
  elseif value >= vector(len)
    nearest = vector(len);
    index   = len;
  else
    indicies = find(vector > value);  
    index    = indicies(1);
    delta_lo =   value - vector(index-1);
    delta_hi = - value + vector(index)  ;
    if delta_lo <= delta_hi
      nearest = vector(index-1);
      index = index-1;
    else
      nearest = vector(index);
    end
  end
end


      
    
    
    
  
