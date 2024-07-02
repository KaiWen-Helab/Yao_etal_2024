% For drawing heatmap of dF/F0 magnitude.
% The data pre-processed and exported from Inper software (xlsx file) was utilized
% Each row represents one trial
% Created by HKW, motified by XRY 

clc;clearvars;
close all;
cd 'file_dir'; 

excel_fn = dir('*.xlsx');   
filenames = {excel_fn.name};
n_excel = length(filenames)
sheetName = 'Detail DFF'
% Change this to desired DPI
resolution = 600; 


for a=1:n_excel
    idxColumns = []
    newXTicks = []
    if strfind(filenames{a},'.xlsx')    
        cellContents = filenames{a};
         %Save name of raw data files
        dataname{a} = cellContents(1:end-5)
        [~, idx, rawData] = xlsread(filenames{a}, sheetName); 
        % Identify columns with titles starting with 'Trail'
        idxColumns = contains(idx(1,:),'Trail');
        % Extract rows 42 to 400 from trial columns
        extractedData = rawData(2:400, idxColumns);
        dataMat = transpose(cell2mat(extractedData));
        
        %% draw heatmap of all trials
        imagesc(dataMat*100);
        % Display range for dFF manitude
        caxis([-2,20]);
        c = colorbar;

        colormap('jet');
        drawnow;
        Ticks = [40, 80, 120, 160, 200, 240, 280, 320, 360, 400];
        set(gca, 'YTick', Ticks/2, 'FontName', 'Arial', 'FontSize', 8);  
        set(gca,'xtick',[]);
        drawnow;
        
        % Draw dotted vertical line at x = 0
        x_line = 200;
        ylim = get(gca, 'YLim');
        y_line = linspace(ylim(1), ylim(2), 100);
        line([x_line, x_line], [ylim(1), ylim(2)], 'LineStyle', '--', 'Color', 'k', 'LineWidth', 1);
        drawnow;
        
        %Save the heatmap
        %name the heatmap as the name of raw data file
        figName = strcat(string(dataname{1,a}),'-heatmap')
        % Set the image size, uint:cm
        set(gcf,'PaperUnits','centimeters','PaperPosition',[0 0 9 6]); 
        %save plot
        print(char(figName), '-dsvg', ['-r',num2str(resolution)]);  
        
    end
end
