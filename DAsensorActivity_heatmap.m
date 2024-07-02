% For drawing DA sensor activity dFF heatmap for a representative mouse at diversed time-points
% dFF value was calculated by 'Fiber_meanActivity.m' in advance
% Sequences of dFF value (2-minute long in this case) at diversed time-points(4wpi,7wpi,10wpi,14wpi)of the same were put in a xlsx file by column in advance
% Created by XRY

cd 'file_dir';
[num, txt, raw] = xlsread('');% path of xlsx file to be analyzed
 
figure;
imagesc(transpose(num));
axis off;
colormap; 
colorbar;
% Display range for dFF magnitude
caxis([-0.05,0.2]);
title('Title');
%save plot
saveas(gcf, 'Image_name', 'svg') 

colorbar off
title(''); 
