%For analyzing fiber photometry mean activity.
%Input data format: csv file with first column as timestamp. rawF data from each recording with the same duration are listed in individual column.
%Calculate dF/F0 by using 20% (can be changed) quantile of each frag as F0.
%Frag length is determined by bin size and can be changed.
%Use 5sec frag to determine the minimal RMS of each recording.
%Calculate sum activity of all dF/F0 above 3xRMS and then mean activity.
%Save both plots for raw F and dF/F0.  
%Save dF/F0 data in sheet1, R(correlation between rawF and dF/F0) in sheet 2 row 2,mean activity in sheet 2 row 3, normalized activity (to the 1st column value) in sheet 2 row 4. 
%Created by HKW, motified by XRY 

clc;clearvars;
clear all
cd 'file_dir';  

csv_fn = dir('*.csv');
filenames = {csv_fn.name};
n_csv = length(filenames)
jj=0

for a=1:n_csv
    dFFdata=[]
    meanSumCell=[]
    R=[]
    
    if strfind(filenames{a},'.csv')    %如果是xls文件格式 注意括号要使用cell的括号
        cellContents = filenames{a};
        dataname{a} = cellContents(1:end-4);
        
        jj=jj+1;
        rawData{jj}=readtable(filenames{a},'ReadRowNames', true);
        rawF = rawData{jj}{:,:};
        [m,n] = size(rawF);
        fs = floor(m/600);      %For 10 min (600s) recording. Change if record with different length
        TS=0
        
        for b=1:n
            TS=12+(b-1)*3
            SW=rawF(:,b);
            
            figure;
            plot(SW(1:580*fs));   %plot raw F in figure
            rawfigName = strcat(string(dataname{1,a}),'-ZT',string(TS),'-rawF')
            saveas(gca,char(rawfigName),'jpeg');   %save plot.
            close all;
            
            yy = [];      
            
            %% Calculate deltaF/F0
            Frag = []
            mFrag = 0
            qtFrag = 0
            minFrag = 0
            dFF_Frag = []
            y = []            
            
            F = SW;
            bin= 8*fs                                    %bin length is 8s 
            fragCount=floor(m/bin)
            %% dFF calculation
            
            for c =1:fragCount
                Frag = F(((c-1)*bin+1):c*bin)           %divide the whole fluorescence into fragments.
                mFrag = mean(Frag);
                qtFrag = quantile(Frag,0.2)        %find 20% quantile of the fragment fluorescence for the cumulative probability or probabilities p in the interval [0,1].
                minFrag = min(Frag);                  %find the minimum value of the fragment, suitable for PV Ca images analysis
                dFF_Frag = (Frag-qtFrag)/qtFrag;    %extract dF/F for each fragments
                y = [y,dFF_Frag]
            end
            %%
            y = reshape(y,[],1);
            dFFdata=[dFFdata,y]
            
          r=[corrcoef(F(1:fs*580,:),y(1:fs*580,:))];      %calculate R2 between rawF and dFF
            R(b)=r(2);
            
            %%calculate sum(deltaF/F0)
            rmsFrag=50      %use 50 datapoint to estimate rms
            rmsSec=0
            minRms = 10
            minRmsLoc=0
            for d=1:(fragCount*bin-rmsFrag)       %find the min RMS of each cell
                sec=d+rmsFrag
                rmsSec=rms(y(d:sec))
                if minRms>rmsSec
                    minRms=rmsSec
                    minRmsLoc=d
                else
                    minRms=minRms
                    minRmsLoc=minRmsLoc
                end
            end
            rmsCell(b)=minRms;
            rmsCellLoc(b)=minRmsLoc;
            
            y_deRms=y(1:fragCount*bin);
            y_deRms(find(y_deRms<3*minRms))=0;
            y_sum=sum(y_deRms)*fs/(fragCount*bin);
            meanSumCell=[meanSumCell,y_sum]; 
            normAct=meanSumCell/meanSumCell(1);
            
            %%plot 580s dff*100
            plot(gca,y(1:fs*580,:)*100,'g');
            ylim([-10,20 ]);
            % Modify x-axis labels by divided by fs
            xTicks = get(gca, 'XTick');
            newXTicks = round(xTicks/(fs),-1)
            set(gca, 'XTickLabel', newXTicks);
            xlabel('Time (s)');
            ylabel('deltaF/F0 (%)');
            
            dFF_figName = strcat(string(dataname{1,a}),'-ZT',string(TS),'-dFF')
            saveas(gca,char(dFF_figName),'jpeg');   %save plot.
            close all;
            
            
        end
        
        %%Save dF/F0 data and sum activity
        name_y = strcat(string(dataname{1,a}),'_dFF','.xlsx')      %define exported excel name
        xlswrite(name_y,dFFdata);                %write dF/F into sheet1 of exported excel
        rowTitles = {'R', 'Mean activity', 'Normalized activity'};
        xlswrite(name_y,R,'Sheet2','B2');
        xlswrite(name_y,rowTitles','Sheet2','A2 ')
        xlswrite(name_y,meanSumCell,'Sheet2','B3 ')
        xlswrite(name_y,normAct,'Sheet2','B4 ')
        
    end
    
    
end



