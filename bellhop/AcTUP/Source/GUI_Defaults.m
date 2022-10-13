function DfltInfo = GUI_Defaults()
%DfltInfo = GUI_Defaults()
%This function sets up some default parameters for the GUI functions that can be user specific.
%Modify this file as required, rename it as a .m file, and put it somewhere on your Matlab path.
%DON'T put it in the GUI library directory on the server as this will muck others around.
 
%Default directory for saving TIF images
DfltInfo.ImageDir = 'C:\0todayswork\Thesis'; 

%Default fonts for title, labels and axis numbering

DfltInfo.TitleFont.FontName = 'Helvetica';
DfltInfo.TitleFont.FontAngle = 'normal';
DfltInfo.TitleFont.FontWeight = 'bold';
DfltInfo.TitleFont.FontUnits = 'points';
DfltInfo.TitleFont.FontSize = 14;


DfltInfo.LabelFont.FontName = 'Helvetica';
DfltInfo.LabelFont.FontAngle = 'normal';
DfltInfo.LabelFont.FontWeight = 'bold';
DfltInfo.LabelFont.FontUnits = 'points';
DfltInfo.LabelFont.FontSize = 12;


DfltInfo.AxisFont.FontName = 'Helvetica';
DfltInfo.AxisFont.FontAngle = 'normal';
DfltInfo.AxisFont.FontWeight = 'normal';
DfltInfo.AxisFont.FontUnits = 'points';
DfltInfo.AxisFont.FontSize = 12;
