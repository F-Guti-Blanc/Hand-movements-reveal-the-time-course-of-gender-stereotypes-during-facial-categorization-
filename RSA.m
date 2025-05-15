%% Hand movements reveal the time course of gender stereotypes during facial categorization  
% 
% Francisco Gutiérrez-Blanco, Ana F. Palenciano and María Ruz
% Department of Experimental Psychology
% Mind, Brain and Behavior Research Center
% University of Granada. Spain


%% Load the Data frame

xposlong = readtable('......./xpos_long.csv');


%% tRDM - Gender Stereotype

% Design of the theoretical representational dissimilarity matrix of gender
% stereotype.

tRDM = [0,1,1,0;
        1,0,0,1;
        1,0,0,1;
        0,1,1,0];

           
%% Mean xpos per participant, per stimulus condition and time step

% stimulus condition 

%     HE = Angry Male
%     HF = Happy Male
%     ME = Angry Female
%     MF = Happy Female

% Number of Participants = 26
% Number of Steps = 100


xpos_HE = zeros (26,100);
xpos_HF = zeros (26,100);
xpos_ME = zeros (26,100);
xpos_MF = zeros (26,100);


for a = 1:26 % number of participants
    for b = 1:100 % number of steps
     
      indices = xposlong.subject_nr == a & xposlong.steps == b;

        xpos_HE_values = xposlong(indices & strcmp(xposlong.TIPOESTIMULO, 'HE'), 'xpos');
        xpos_HF_values = xposlong(indices & strcmp(xposlong.TIPOESTIMULO, 'HF'), 'xpos');
        xpos_ME_values = xposlong(indices & strcmp(xposlong.TIPOESTIMULO, 'ME'), 'xpos');
        xpos_MF_values = xposlong(indices & strcmp(xposlong.TIPOESTIMULO, 'MF'), 'xpos');
        
        if ~isempty(xpos_HE_values)
            xpos_HE(a, b) = mean(table2array(xpos_HE_values));
        end
        if ~isempty(xpos_HF_values)
            xpos_HF(a, b) = mean(table2array(xpos_HF_values));
        end
        if ~isempty(xpos_ME_values)
            xpos_ME(a, b) = mean(table2array(xpos_ME_values));
        end
        if ~isempty(xpos_MF_values)
            xpos_MF(a, b) = mean(table2array(xpos_MF_values));
        end
    end
end

clear xpos_HE_values xpos_HF_values xpos_ME_values xpos_MF_values a b ind indices

%% eRMD´s

% Design of empirical representational dissimilarity matrices
% per participant and time step


eRDMs= cell(1,26);

for i = 1:26
   eRDMs{i} = cell(1,100);
    for j = 1:100
        eRDMs{i}{j}= cell(0);
    end
end

clear i j 


for i = 1:26
    for j = 1:100
    a = xpos_HE(i,j);
    b = xpos_HF(i,j);
    c = xpos_ME(i,j);
    d = xpos_MF(i,j);
        m = [a-a,a-b,a-c,a-d;
             b-a,b-b,b-c,b-d;
             c-a,c-b,c-c,c-d;
             d-a,d-b,d-c,d-d];
        eRDMs{i}{j} = abs(m);
    end
end

 clear a i j m  b c d xpos_MF xpos_ME xpos_HE xpos_HF 

%% Spearman's correlations


Table_Corr= zeros (26,100);

for i = 1:26
    for j = 1:100
        tRDM_c = squareform(tRDM);
        eRDMs_c = squareform(eRDMs{i}{j});
        Table_Corr(i,j)= corr(eRDMs_c', tRDM_c','type','Spearman');
    end
end


clear i j eRDM_c tRDM_c

%% Wilcoxon signed rank test, one tail

TEST = zeros (100:5);

for i = 2:100
    [p,h,stats] = signrank(Table_Corr(:,i),0,'tail','right');
    TEST(i,1) = stats.zval;
    TEST(i,2) = p;
    TEST(i,3) = h;
end

p_values = TEST(:,2);


%%  Multiple comparison correction, FDR method.

% Due to the version of Matlab we use, it does not have the necessary 
% toolbox to run the FDR multiple comparisons correction.
% We export the p-values to R, where we run the corrections (see RSA.r).
% No he conseguido la app de MATLAB que las hace 
% La funcion de MATLAB es "mafdr"

fileID = fopen('p_values.txt', 'w');
fprintf(fileID, '%f\n', p_values);
fclose(fileID);

% Load the corrected FDR file in R

load adjusted_p_values.txt;

TEST(:,5)= adjusted_p_values(:,1);


% Calculate the average time in each of the steps and incorporate them into TEST.

for i = 1:100
    TEST(i,4) = mean(xposlong.timestamps(xposlong.steps==i));
end

%% Plot Figure 8

figure;

plot(TEST(:,4),TEST(:,1),'LineWidth', 6);
hold on 
color_line = 'k';
indices = TEST(2:end,5) < 0.05;
x_points = TEST(indices, 4);
y = max(max(TEST(:,1))) * 1.15;
scatter(x_points, repmat(y, sum(indices), 1), 20, color_line, 'filled');

grid on;
hold off;

