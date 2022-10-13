% AcToolboxFrontEnd
% User friendly matlab front-end for Mike Porter's Acoustics Toolbox
%
% Author:
% Alec J Duncan, 
% Centre for Marine Science and Technology, 
% Curtin University of Technology,
% Kent Street, Bentley, Western Australia
% a.duncan@cmst.curtin.edu.au
%
% Provides a uniform user interface for running and plotting the results from the following programs:
% kraken    (normal mode model) 
% krakenc   (complex normal mode model)
% scooter   (fast-field model)
% bellhop   (gaussian beam tracing)
% bounce    (bottom reflection coefficient from geoacoustic model)
%
% This is especially handy for running models at multiple frequencies or for running different models on the same
% problem. 
%
% The price for all this is, however, some loss of flexibility, as to keep the interface as simple as possible quite
% a lot of the parameters and options are hard-coded or computed by the program.
%
% This interface can only run kraken, krakenc and scooter for range independent problems.
% Bellhop can be run for range dependent bathymetry but not range dependent sound speed or bottom properties.
%
% The interface can only handle single source depths, but it can handle multiple receiver depths, multiple
% receiver ranges and multiple frequencies.
%
% If you get an obscure error message coming up in the command window check the appropriate .prn file for 
% more meaningful error messages.  Consult the OALIB ...\doc\*.hlp files for further information.
%
% The Acoustic Toolbox is available from:
% ftp://oalib.saic.com/pub/oalib/AcousticsToolbox/
%
% Code has been tested with the version of the Acoustics Toolbox contained in:
%  atPII_f95.zip    June 27th 2002
%
% Works with Matlab 5 and Matlab 6 although Matlab 6 has a bug which makes some of the GUI windows sometimes only partly visible.
% Switching back and forth between windows usually solves the problem.
%
%*****************************************************************************************************************************
% To use it:
% 1. download and unzip the Acoustics Toolbox (atPII_f95.zip, see above) into a convenient directory
% 2. Unzip the AcToolboxFrontEnd archive into an appropriate directory and either 
%    put this directory on your matlab path or cd to it from within matlab (don't put @AcEnvironment or @AcLayer on the path)
% 3. Use your matlab editor or another text editor to modify GetAcDirectoryInfo.m to specify the correct locations
%    for the Acoustic Toolbox files and the working directories you want to use.
% 4. At the Matlab command prompt, type act (this calls AcToolboxFrontEnd.m) 
% 5. Hopefully from here on the menus and prompts will make it straightforward.
%
% The program starts up with a default run definition which you can run the various propagation models on as is, or
% you can modify the run definition to suit your own problems.  There are also a number of pre-defined run definitions that
% you can load, modify etc. - many of these are examples from "Computational ocean Acoustics" by Jensen, Kuperman, 
% Porter and Schmidt.  Page numbers refer to the Springer 2000 edition - AIP Press edition page numbers are slightly different.  
%
% You specify a directory and file prefix for each run and the program appends the frequency and an extension to make 
% the file name.  Files with a number of different
% extensions are generated as follows:
%   .env  Environment file used when running the specified propagation program (all)
%   .prn  File used to log program output - check this if any errors occur (all)
%   .shd  Shade file, ie. acoustic transmission loss data (kraken, krakenc, scooter, and bellhop
%         if told to compute transmission loss)
%   .brc  Bottom reflection coefficient file - this in the format that bellhop likes (bounce and bellhop)
%   .ray  Ray file (bellhop if told to compute rays or eigenrays)
%   .arr  Amplitude/delay information for each arrival (bellhop if told to compute arraivals information)
%   .grn  Depth dependent Greens function (Scooter)
%   .mod  Mode file (kraken, krakenc)
%
% Input dialog boxes can handle valid matlab expressions as well as numeric values.  This is particularly useful
% for specifying  a vector of values, for example:
% 30:5:200
% specifies the vector [30, 35, 40, ... , 200]
%
% Sound speed profiles can either be specified manually or read from an ascii text file.
% Each row of file has following fields:
% Depth Cp Density Cs Ap  As
%
% where:
% Depth is depth below top of layer in metres
% Cp is compressional sound speed in m/s
% Density is density in kg/m^3
% Cs is shear sound speed in m/s
% Ap is compressional wave absorption in dB/wavelength
% As is shear wave absorption in dB/wavelength
%
% You don't have to specify all columns, any after the last specified will be set to defaults: 
% Cp = 1500m/s, Density = 1024m/s, Cs, Ap, As = 0
%
% Bellhop can optionally be run with a bathymetry file for problems with bathynmetry dependent on range although the
% geoacoustic parameters and water column sound speed profile are treated as independent of range.
% The format of the bathymetry (.bty) file is as follows:
%   N
%   R(1) Zw(1)
%   R(2) Zw(2)
%    .     .
%    .     .
%    .     .
%   R(N)  Zw(N)
%
% Where: N is the number of range - depth pairs
%       R is the range in km
%       Zw is the water depth in metres
%
% Note that in this case the water column sound speed profile in the environment definition must be specified down to 
% the deepest depth in the bathymetry out to the maximum range specified for the run.
%
% I've put a lot of error trapping in the code, but there may still be conditions that aren't trapped.  Please contact me
% at the above email address if you find any bugs or have any suggestions for improvements etc.
%
% **********************************************************************************************************
% Release notes:
% Changes incorporated in Version 1.1  (29 May 2002):
% - Added capability to read sound speed profiles from an ascii text file
%
% Changes incorporated in Version 1.2 (12th June 2002)
% - Fixed some bugs to do with default directories to make it easier to transfer run definitions
%   between machines
% - Fixed a bug in Greens function reading code (ReadGreen.m) that caused initial block of data to be
%   overwritten
% - Added act.m which simply calls actoolboxfrontend.m - for those who don't like typing!
% - Added additional error checking to prevent aborts on some types of directory creation errors
%   and aborts when trying to read invalid shade files
% - Added support for additional Bellhop run options
% - Added automatic scanning of log (.prn) files for error and warning messages
% - Number of mesh points in environment file is now set to zero so that propagation code will pick
%   a default value
% - Added option to allow manual editing of automatically generated environment files, .flp files etc.
%   prior to the code being run
%
% Changes incorporated in Version 1.3  (13th June 2002)
% - Code now reads binary shade files instead of converting to ascii
% - Added option to force exhaustive mode search when running Krakenc
% - Fixed problem with directory paths containing blanks
%
% Changes incorporated in Version 1.4 (14th June 2002)
% - Improved method of specifying layers so they automatically stack
% - Directory for each run definition is now specified relative to main working directory
% - added option to plot environment parameters (still a bit untidy)
%
% Changes incorporated in Version 1.5 (27th June 2002) 
% - Fixed bug in error screening code that caused problems when there are a large number of warnings in .prn file
% - Code now consistently keeps intermediate files (bottom reflection coefft, mode, greens function) 
% - Added capability to plot mode locations in complex Kr plane
% - Added capability to plot mode shapes
% - Added capability to plot amplitude - delay information produced by bellhop with 'A' option
% - Added screening for run type and beam type combinations not suppoprted by bellhop
%
% Changes incorporated in Version 1.6 (1st July 2002) 
% - Added capability to read a run definition from a kraken or bellhop environment file
% - Tidied up environment plotting