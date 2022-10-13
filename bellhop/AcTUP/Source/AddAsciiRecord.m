function errmsg = AppendAsciiFileRecord(fname, string, delim);
%AppendAsciiFileRecord      Add record to ASCII file
%                           note can also be used for appending fields (change delim)
%
% USAGE:    status = AppendAsciiFileRecord(fname, string, delim)
%
% IP        fname        target file (inc path)
%           string       record OR cell array of records
%           delim        delimiter between each record
%                        = '\n' DEFAULT
%                        IP can be either string or ascii value of delim character
%                        
% OP        errmsg       = '' no message
%
% Revision 0.0    10 December 2004 ... ALM
%                 - i'm sure there' s a matlab function around somewhere that does this better


errmsg = '';
head   = 'ERROR -> AppendAsciiFileRecord: ';

if ~exist('delim')
   delim = '\n';
elseif isnum(delim)
   delim = char(delim);
elseif ~ischar(delim)
   delim = '\n';
end

if ischar(string)
   string = {string};
end

if exist(fname, 'file')   
   errcode = 0;
   fout = fopen(fname, 'a');
   if fout == -1 then
      errmsg = [head, 'fopen failure on file ', fname];
   else
      try
         for ii = 1:length(string),
            fprintf(fout, '%s%s', string{ii}, delim);
         end
      catch
         errmsg = [head, lasterr];
      end
      fclose(fout);
   end
else
   errmsg = [head, 'File ', fname, ' not found'];
end
