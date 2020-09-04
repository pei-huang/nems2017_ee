%% Load in distributions
% For old vs new homes
homeprojec = readtable('OldVsNew.csv');
% For outdoor conc from EPA, approximate as normal distribution
std_up = (9.615-7.617)/1.28;
std_low = (7.617-5.196)/1.28;
std_C0 = (std_up+std_low)/2;
%pd_C0 = makedist('Normal','mu',7.62,'sigma',std_C0); % Use this for panels
%a and b
pd_C0 = makedist('Uniform','lower',1,'upper',35); % Use this for panels c and d
% For penetration factor
pd_P = makedist('Uniform','lower',0.8,'upper',0.9);

%% Load in future projections
load('relach.mat');
rel_a_inf = V;
clear V
%% Run simulation 
p = 1; % Loop progress

k_dep = 0.3; % 1/hr 

% Choose model years, EE scenarios, and simulation runs
model_years = [4 38]; % 1 is 2013, 38 is 2050
model_scen = [1 2 3]; % 1 is reference, 2 is intermediate, 3 is optimisitic
model_runs = 10000000; % Number of homes to simulate

model_vars = {'C_ss','year','scen','home_type','a_inf','a_nat','house_vol','household_size','E','P','C0','f_hvac','a_recirc','eff_filt'};
sim_mat = zeros(length(model_years)*length(model_scen)*model_runs,length(model_vars));

jj = 1;
for year = model_years 
    for scen = model_scen 
            for i = 1:model_runs
                % Randomly generate the values required to run the model
                % Decide whether it will be an old or new home
                [home_type] = DetermineHome(homeprojec,year);
                % Then determine the the infiltration rate, square footage, natural
                % ventilation, household size etc. 
                [a_inf,a_nat] = Ventilation(year,home_type,rel_a_inf,scen);
                % Determine house size
                [house_vol] = HouseVolume(year);
                % Determine household size
                [household_size] = HouseholdSize();    
                % Then determine the indoor emission rate
                [E] = Emissions(household_size,year);                   
                % Then determine the penetration factor
                [P] = random(pd_P);
                % Then determine the outdoor ambient concentration 
                [C0] = random(pd_C0);
                if C0 < 1
                    C0 = 1; % Lower limit
                end
                % Determine HVAC recirculation terms
                [f_hvac,a_recirc,eff_filt] = HVAC(home_type,year,scen);

                % Then calculate the steady state concentration
                C_ss = ((a_inf*P+a_nat)*C0+E/house_vol)/(a_inf+a_nat+k_dep+f_hvac*a_recirc*eff_filt);

                % Finally calculate the efolding time
                efold = EFoldTime(C_ss,a_inf,a_nat,P,C0,house_vol,k_dep,a_recirc,eff_filt,E);
                
                % Save the data with tags determining 
                sim_mat(jj,:) = [C_ss,year,scen,home_type,a_inf,a_nat,house_vol,household_size,E,P,C0,f_hvac,a_recirc,eff_filt];
                jj = jj + 1;
            end
        fprintf('Progress: %.2f \n',p/(length(model_years)*length(model_scen))*100);
        p = p + 1;    
    end
end

sim_data = array2table(sim_mat,'VariableNames',model_vars);
save('PanelCD10millBoth.mat','sim_data')

%% Summarize results
y_2016_ref = sim_data.year==4&sim_data.scen==1;
y_2050_ref = sim_data.year==38&sim_data.scen==1;
y_2050_int = sim_data.year==38&sim_data.scen==2;
y_2050_opt = sim_data.year==38&sim_data.scen==3;

% Conc data
conc_data = [sim_data.C_ss(y_2016_ref), sim_data.C_ss(y_2050_ref), sim_data.C_ss(y_2050_int),sim_data.C_ss(y_2050_opt)];
labels = {'2016','2050 Ref','2050 Int','2050 Opt'};

figure
boxplot(conc_data,labels)
xlabel('Scenarios')
ylabel('Indoor Concentration (ug/m^{3})')

% Emissions data
E_data = [sim_data.E(y_2016_ref), sim_data.E(y_2050_ref), sim_data.E(y_2050_int),sim_data.E(y_2050_opt)];
figure
boxplot(E_data,labels)
xlabel('Scenarios')
ylabel('Weekly Indoor Emission Rate (ug/hr)')
% Infiltration data
a_data = [sim_data.a_inf(y_2016_ref), sim_data.a_inf(y_2050_ref), sim_data.a_inf(y_2050_int),sim_data.a_inf(y_2050_opt)];
figure
boxplot(a_data,labels)
xlabel('Scenarios')
ylabel('Infiltration Rate (1/hr)')
% Natural ventilation 
nat_data = [sim_data.a_nat(y_2016_ref), sim_data.a_nat(y_2050_ref), sim_data.a_nat(y_2050_int),sim_data.a_nat(y_2050_opt)];
figure
boxplot(nat_data,labels)
xlabel('Scenarios')
ylabel('Natural Ventilation Rate (1/hr)')
% Outdoor Concentration
outdoor_data = [sim_data.C0(y_2016_ref), sim_data.C0(y_2050_ref), sim_data.C0(y_2050_int),sim_data.C0(y_2050_opt)];
figure
boxplot(outdoor_data,labels)
xlabel('Scenarios')
ylabel('Outdoor Conc (ug/m^{3})')
% HVAC rate
hvac_data = [sim_data.a_recirc(y_2016_ref), sim_data.a_recirc(y_2050_ref), sim_data.a_recirc(y_2050_int),sim_data.a_recirc(y_2050_opt)];
figure
boxplot(hvac_data,labels)
xlabel('Scenarios')
ylabel('Recirculation Rate (1/hr)')

% Calculate mean indoor conc rather than median
avg_indoor_conc = mean(conc_data);
st_dev = std(conc_data);
figure
errorbar(1:4,avg_indoor_conc,st_dev)
ylabel('Indoor Concentration Mean and St Dev (ug/m^{3})')
xlabel('Scenarios')
xlim([0 5])

% Check for new vs old homes
y_2016_ref_old = sim_data.year==4&sim_data.scen==1&sim_data.home_type==1;
y_2016_ref_new = sim_data.year==4&sim_data.scen==1&sim_data.home_type==2;
y_2050_ref_old = sim_data.year==38&sim_data.scen==1&sim_data.home_type==1;
y_2050_ref_new = sim_data.year==38&sim_data.scen==1&sim_data.home_type==2;
y_2050_int_old = sim_data.year==38&sim_data.scen==2&sim_data.home_type==1;
y_2050_int_new = sim_data.year==38&sim_data.scen==2&sim_data.home_type==2;
y_2050_opt_old = sim_data.year==38&sim_data.scen==3&sim_data.home_type==1;
y_2050_opt_new = sim_data.year==38&sim_data.scen==3&sim_data.home_type==2;

%% Emissions percentile graph
clear sim_index E_pct C_avg_pct C_ratio_pct
% Logicals for old and new homes for 2016 and 2050 under the scenarios
q = 1;
for year_i = [4 38]
    for scen_i = [1 2 3]
        for home_type_i = [1 2]
            sim_index(:,q) = sim_data.year==year_i&sim_data.scen==scen_i&sim_data.home_type==home_type_i;
            q = q + 1;
        end
    end
end

% Calculate emission percentiles
q = 1;
for year_i = [4 38]
    for scen_i = [1 2 3]
        for home_type_i = [1 2]
            E_pct(:,q) = prctile(sim_data.E(sim_index(:,q)),[5 15 25 35 45 55 65 75 85 95])';
            q = q + 1;
        end
    end
end

% Average the concentrations within those percentiles
q = 1;
for year_i = [4 38]
    for scen_i = [1 2 3]
        for home_type_i = [1 2]
            C_ss_i = sim_data.C_ss(sim_index(:,q));
            E_i = sim_data.E(sim_index(:,q));
            
            for pct = 1:9 % now loop over the 9 bins
                E_bin_index = E_i>=E_pct(pct,q)&E_i<E_pct(pct+1,q);
                C_avg_pct(pct,q) = mean(C_ss_i(E_bin_index));
            end
            q = q + 1;
        end
    end
end

% Now calculate ratios
q = 1;

for scen_i = [1 2 3]
    for home_type_i = [1 2]
        for pct = 1:9
            C_ratio_pct(pct,q) = C_avg_pct(pct,q+5)/C_avg_pct(pct,q);
        end
        q = q + 1;
    end
end
            
%% Plot
X = 0.1:0.1:0.9;
darkRed = [139 0 0]/255;
darkGreen = [0 100 0]/255;
darkYellow = [153 153 0]/255;
color = {darkRed,darkYellow,darkGreen};

figure
l = 1;
for i = [1 3 5]
    plot(X,C_ratio_pct(:,i),'-o','Color',color{l})
    hold on
    plot(X,C_ratio_pct(:,i+1),'-s','Color',color{l})
    l = l + 1;
    hold on
end
ylabel('C_{2050}/C_{2016}')
xlabel('Emission Decile')
h = zeros(5, 1);
h(1) = plot(NaN,NaN,'o','Color','k');
h(2) = plot(NaN,NaN,'s','Color','k');
h(3) = plot(NaN,NaN,'-','Color',darkRed,'LineWidth',2.5);
h(4) = plot(NaN,NaN,'-','Color',darkYellow,'LineWidth',2.5);
h(5) = plot(NaN,NaN,'-','Color',darkGreen,'LineWidth',2.5);
legend(h, 'Old Homes','New Homes','Reference','Intermediate','Optimistic','Orientation','horizontal')

%% Load in all of the data
sim_data = [];
load('FinalRun1.mat');
sim_1 = sim_data;
sim_data = [];
load('FinalRun2.mat');
sim_2 = sim_data;
sim_data = [];
load('FinalRun3.mat');
sim_3 = sim_data;
sim_data = [];

sim_data = [sim_1;sim_2;sim_3];
%% Grab emission values

scen_data = [];

for year = [4 8 18 28 38] % Run the simulation for 2016 and 2050
    scen_data = [];
    for scen = 1:3 % Run for each scenario we consider
        scen_k = [];
        for kk = 1:length(E_numbers)
            row_k = [];
            index_i_recric = sim_data.year==year&sim_data.scen==scen&sim_data.a_recirc~=0&sim_data.k==kk;
            index_i_norec = sim_data.year==year&sim_data.scen==scen&sim_data.a_recirc==0&sim_data.k==kk;
            C_ss_recirc_avg = mean(sim_data.C_ss(index_i_recric));
            std_recirc = std(sim_data.C_ss(index_i_recric));
            C_ss_rorec_avg = mean(sim_data.C_ss(index_i_norec));
            std_norec = std(sim_data.C_ss(index_i_norec));
            
            row_k = [C_ss_recirc_avg,std_recirc,C_ss_rorec_avg,std_norec];
            scen_k = [scen_k;row_k];
        end
        scen_data = [scen_data scen_k];
    end
    % Write Excel file before going to the next year
    filename = strcat('Year',num2str(year),'.csv');
    csvwrite(filename,scen_data)
end
%% Make emission deciles plot

data_2016 = csvread('Year4.csv');
data_2020 = csvread('Year8.csv');
data_2030 = csvread('Year18.csv');
data_2040 = csvread('Year28.csv');
data_2050 = csvread('Year38.csv');

ratios(:,:,1) = data_2016./data_2016;
ratios(:,:,2) = data_2020./data_2016;
ratios(:,:,3) = data_2030./data_2016;
ratios(:,:,4) = data_2040./data_2016;
ratios(:,:,5) = data_2050./data_2016;


%% Plot
ratios = data_2050./data_2016;
X = 0.05:0.1:0.95;
darkRed = [139 0 0]/255;
darkGreen = [0 100 0]/255;
darkYellow = [153 153 0]/255;
color = {darkRed,darkYellow,darkGreen};

figure
l = 1;
for i = [1 5 9]
    plot(X,ratios(:,i),'-o','Color',color{l},'LineWidth',2.5)
    l = l + 1;
    hold on
end
plot([0 1],[1 1],'--k')
ylabel('Relative Indoor Change (C_{2050}/C_{2016})')
xlabel('Indoor Emission Percentile')
h = zeros(3, 1);
h(1) = plot(NaN,NaN,'-','Color',darkRed,'LineWidth',2.5);
h(2) = plot(NaN,NaN,'-','Color',darkYellow,'LineWidth',2.5);
h(3) = plot(NaN,NaN,'-','Color',darkGreen,'LineWidth',2.5);
legend(h, 'Reference','Intermediate','Optimistic','Orientation','horizontal')
title('All Homes with Recirculation')
ylim([0.75 1.25])

figure
l = 1;
for i = [1 5 9]
    plot(X,ratios(:,i+2),'-s','Color',color{l},'LineWidth',2.5)
    l = l + 1;
    hold on
end
plot([0 1],[1 1],'--k')
ylabel('Relative Indoor Change (C_{2050}/C_{2016})')
xlabel('Indoor Emission Percentile')
h = zeros(3, 1);
h(1) = plot(NaN,NaN,'-','Color',darkRed,'LineWidth',2.5);
h(2) = plot(NaN,NaN,'-','Color',darkYellow,'LineWidth',2.5);
h(3) = plot(NaN,NaN,'-','Color',darkGreen,'LineWidth',2.5);
legend(h, 'Reference','Intermediate','Optimistic','Orientation','horizontal')
title('All Homes without Recirculation')
ylim([0.75 1.25])
%% Contour plot
contour_index_ref = sim_data.scen==1&sim_data.year==38&sim_data.C0>0&sim_data.C_ss>0;
E_cont = sim_data.E(contour_index);
C_ss_cont = sim_data.C_ss(contour_index);
C_amb_cont = sim_data.C0(contour_index);


x = E_cont;
y = C_amb_cont;
z = C_ss_cont;
% Grid 
x0 = min(x) ; x1 = max(x) ;
y0 = 0 ; y1 = 20 ;
N = 20 ;
xl = linspace(x0,x1,N) ; 
yl = linspace(y0,y1,N) ; 
[X,Y] = meshgrid(xl,yl) ;
%%do inteprolation 
P = [x,y] ; V = z ;
F = scatteredInterpolant(P,V) ;
F.Method = 'natural';
F.ExtrapolationMethod = 'linear' ;  % none if you dont want to extrapolate
% Take points lying insuide the region
pq = [X(:),Y(:)] ; 
vq = F(pq) ;
Z = vq ;
Z = reshape(Z,size(X)) ;
figure
surf(X,Y,opt_surf-ref_surf) ;
view([0 90])
xlim([0 x1])
ylim([0 y1])
ylabel('Ambient Outdoor PM_{2.5} Concentration (ug/m^{3})')
xlabel('Indoor Emission Value (ug/hr)')
title('Optimistic EE Scenario, Year 2050')
c = colorbar;
c.Label.String = 'Optimistic - Reference (ug/m^{3})';

%% Make time series concentration plot
years = [2016 2020 2030 2040 2050];
figure

for vent_type = 1:2
    figure
    for scen = 1:3
        scen_conc = [];
        scen_std = [];
        for year = [4 8 18 28 38]
            if vent_type == 1
                C_index = sim_data.year==year&sim_data.scen==scen&sim_data.a_recirc~=0&sim_data.k>=7;
            else
                C_index = sim_data.year==year&sim_data.scen==scen&sim_data.a_recirc==0&sim_data.k>=7;
            end
            avg_conc = mean(sim_data.C_ss(C_index));
            dev_conc = std(sim_data.C_ss(C_index));
            scen_conc = [scen_conc,avg_conc];
            scen_std = [scen_std,dev_conc];
        end
        years2 = [years, fliplr(years)];
        inBetween_l = [[scen_conc-scen_std], fliplr(scen_conc)];
        inBetween_u = [scen_conc, fliplr([scen_conc+scen_std])];
        plot(years,[scen_conc-scen_std],'Color',color{scen},'LineStyle','none')
        hold on
        plot(years,scen_conc,'Color',color{scen},'LineWidth',2.5)
        hold on
        l = fill(years2,inBetween_l,color{scen},'LineStyle','none');
        set(l,'facealpha',.25)
        hold on
        plot(years,[scen_conc+scen_std],'Color',color{scen},'LineStyle','none')
        hold on
        h = fill(years2,inBetween_u,color{scen},'LineStyle','none');
        set(h,'facealpha',.15)
    end
    h = zeros(3, 1);
    h(1) = plot(NaN,NaN,'-','Color',darkRed,'LineWidth',2.5);
    h(2) = plot(NaN,NaN,'-','Color',darkYellow,'LineWidth',2.5);
    h(3) = plot(NaN,NaN,'-','Color',darkGreen,'LineWidth',2.5);
    legend(h, 'Reference','Intermediate','Optimistic','Orientation','horizontal')
    xlabel('Year')
    ylabel('Indoor Concentration PM_{2.5} Concentration (ug/m^{3})')
    if vent_type == 1
        title('Homes with Recirculation (Top 30% Indoor Emissions)')
    else
        title('Homes without Recirculation (Top 30% Indoor Emissions)')
    end
        
    hold off
end

%% Response Time Calculation
y_2016_ref = sim_data.year==4&sim_data.scen==1;
y_2050_ref = sim_data.year==38&sim_data.scen==1;
y_2050_int = sim_data.year==38&sim_data.scen==2;
y_2050_opt = sim_data.year==38&sim_data.scen==3;


[efoldT,C_ss,C,E] = ResponseTime(sim_data.a_inf(y_2016_ref),sim_data.a_nat(y_2016_ref),sim_data.a_inf(y_2016_ref)+sim_data.a_nat(y_2016_ref),sim_data.P(y_2016_ref),sim_data.C0(y_2016_ref),sim_data.house_vol(y_2016_ref),0.3,sim_data.a_recirc(y_2016_ref),sim_data.eff_filt(y_2016_ref),3.8e4,1000);


opt_2050_data = sim_data(y_2050_opt);



%% Plot for Panel A
data_2016 = csvread('Year4.csv');
data_2050 = csvread('Year38.csv');
ratios = data_2050./data_2016;
X1 = 1:10;
X2 = 12:21;
ref = [202 108 95]/255;
int = [69 183 205]/255;
opt = [64 177 162]/255;
color = {ref,int,opt};

figure
l = 1;
for i = [1 5 9]
    plot(X1,ratios(:,i),'-o','Color',color{l},'MarkerFaceColor',color{l},'LineWidth',2.5)
    hold on 
    plot(X2,ratios(:,i+2),'-^','Color',color{l},'MarkerFaceColor',color{l},'LineWidth',2.5)
    l = l + 1;
    hold on
end
plot([1 10],[1 1],'--','LineWidth',1.5,'Color',[191 191 191]/255)
hold on
plot([12 21],[1 1],'--','LineWidth',1.5,'Color',[191 191 191]/255)
ylabel('Relative change in indoor PM_{2.5} concentration (C_{2050}/C_{2016})')
xlabel('Indoor Emission Percentile')
h = zeros(3, 1);
h(1) = plot(NaN,NaN,'-','Color',color{1},'LineWidth',2.5);
h(2) = plot(NaN,NaN,'-','Color',color{2},'LineWidth',2.5);
h(3) = plot(NaN,NaN,'-','Color',color{3},'LineWidth',2.5);
legend(h, 'Reference','Intermediate EE','Optimistic EE','Orientation','horizontal')
legend box off
ylim([0.75 1.25])
xlim([0 22])
xticks([X1 X2])
xticklabels({'5','15','25','35','45','55','65','75','85','95','5','15','25','35','45','55','65','75','85','95'})
yticks([0.8 0.9 1.0 1.1 1.2])
set(gca,'TickDir','out');
txt1 = {'Recirculation'};
text(5.5,1.15,txt1,'HorizontalAlignment','center')
txt2 = {'No recirculation'};
text(16.5,1.15,txt2,'HorizontalAlignment','center')

%% Contour plot
contour_index_ref = sim_data.scen==1&sim_data.year==38&sim_data.C0>2&sim_data.C_ss>2;
contour_index_opt = sim_data.scen==3&sim_data.year==38&sim_data.C0>2&sim_data.C_ss>2;
E_cont_ref = sim_data.E(contour_index_ref);
E_cont_opt = sim_data.E(contour_index_opt);
C_amb_ref = sim_data.C0(contour_index_ref);
C_amb_opt = sim_data.C0(contour_index_opt);
C_ss_ref = sim_data.C_ss(contour_index_ref);
C_ss_opt = sim_data.C_ss(contour_index_opt);

% Create Z for ref
x = E_cont_ref;
y = C_amb_ref;
z = C_ss_ref;
% Grid 
x0 = min(x) ; x1 = max(x) ;
y0 = 2 ; y1 = 20 ;
N = 20 ;
xl = linspace(x0,x1,N) ; 
yl = linspace(y0,y1,N) ; 
[X,Y] = meshgrid(xl,yl) ;
%do inteprolation 
P = [x,y] ; V = z ;
F = scatteredInterpolant(P,V) ;
F.Method = 'natural';
F.ExtrapolationMethod = 'none' ;  % none if you dont want to extrapolate
% Take points lying insuide the region
pq = [X(:),Y(:)] ; 
vq = F(pq) ;
Z = vq ;
Z_ref = reshape(Z,size(X)) ;

% Create Z for opt
x = E_cont_opt;
y = C_amb_opt;
z = C_ss_opt;
% Grid 
x0 = min(x) ; x1 = max(x) ;
y0 = 0 ; y1 = 20 ;
N = 20 ;
xl = linspace(x0,x1,N) ; 
yl = linspace(y0,y1,N) ; 
[X,Y] = meshgrid(xl,yl) ;
%do inteprolation 
P = [x,y] ; V = z ;
F = scatteredInterpolant(P,V) ;
F.Method = 'natural';
F.ExtrapolationMethod = 'linear' ;  % none if you dont want to extrapolate
% Take points lying insuide the region
pq = [X(:),Y(:)] ; 
vq = F(pq) ;
Z = vq ;
Z_opt = reshape(Z,size(X)) ;

% OptOverRef
OptOverRef = Z_opt./Z_ref;

figure
surf(X,Y,OptOverRef) ;
view([0 90])
xlim([0 x1])
ylim([0 y1])
ylabel('Ambient Outdoor PM_{2.5} Concentration (ug/m^{3})')
xlabel('Indoor Emission Value (ug/hr)')
c = colorbar;
c.Label.String = 'C_{Opt}/C_{Ref}';

%% Figure Panel B on TimeResponse
clear recirc_index norec_index recirc_pct norec_pct
ref = [202 108 95]/255;
int = [69 183 205]/255;
opt = [64 177 162]/255;
color = {ref,int,opt};

y_2016_efold_recirc = sim_data.year==4&sim_data.scen==1&sim_data.a_recirc~=0;
y_2016_efold_norec = sim_data.year==4&sim_data.scen==1&sim_data.a_recirc==0;


i = 1;
for year = [4 38]
    j = 1;
    for scen = [1 2 3]
        
        recirc_index = sim_data.year==year&sim_data.scen==scen&sim_data.a_recirc~=0;
        norec_index = sim_data.year==year&sim_data.scen==scen&sim_data.a_recirc==0;
        
        efold_recirc = sim_data.efold(recirc_index);
        efold_norec = sim_data.efold(norec_index);
        
        recirc_pct(j,1,i) = mean(efold_recirc);
        recirc_pct(j,2,i) = std(efold_recirc);
        
        norec_pct(j,1,i) = mean(efold_norec);
        norec_pct(j,2,i) = std(efold_recirc);
        
        j = j + 1;
    end
   i = i + 1;
end

recirc_mean = recirc_pct(:,1,2)./recirc_pct(:,1,1); % for mean
norec_mean = norec_pct(:,1,2)./norec_pct(:,1,1); % for mean

recirc_std = sqrt((recirc_pct(:,2,2)./recirc_pct(:,1,2)).^2+(recirc_pct(:,2,1)./recirc_pct(:,1,1)).^2);
norec_std = sqrt((norec_pct(:,2,2)./norec_pct(:,1,2)).^2+(norec_pct(:,2,1)./norec_pct(:,1,1)).^2);

fig = figure;
index = 1;

for scen = [1 2 3]
    s1 = scatter(index-0.05,recirc_mean(scen),'o','filled');
    s1.CData = color{scen};
    hold on
    e1 = errorbar(index-0.05,recirc_mean(scen),recirc_std(scen),recirc_std(scen));
    e1.Color = color{scen};
    s2 = scatter(index+0.05,norec_mean(scen),'^','filled');
    s2.CData = color{scen};
    hold on
    e2 = errorbar(index+0.05,norec_mean(scen),norec_std(scen),norec_std(scen));
    e2.Color = color{scen};
    hold on
    index = index + .3;
end

plot([0 4],[1 1],'--','LineWidth',1,'Color',[191 191 191]/255)
hold on
h = zeros(2, 1);
h(1) = scatter(NaN,NaN,'o','filled','MarkerFaceColor',[127 127 127]/255);
h(2) = scatter(NaN,NaN,'^','filled','MarkerFaceColor',[127 127 127]/255);
legend(h, 'Recirculation','No recirculation');
legend boxoff
hold off
ylim([0.75 2])
ylabel('Relative change in response time (t_{2050}/t_{2016})')
set(gca,'YMinorTick','off')
set(gca,'TickDir','out');
xticks([1 1.3 1.6])
xlim([0.9 1.7])
yticks([1 1.5 2.0])
xlabel('Scenarios')
xticklabels({'Reference','Intermediate EE','Optimistic EE'})
box on

%% Panel C and D and E
clearvars -except sim_data
index_ref = sim_data.scen==1&sim_data.year==38;
index_opt = sim_data.scen==3&sim_data.year==38;

E_ref = sim_data.E(index_ref);
E_opt = sim_data.E(index_opt);
C_ref = sim_data.C0(index_ref);
C_opt = sim_data.C0(index_opt);
C_ss_ref = sim_data.C_ss(index_ref);
C_ss_opt = sim_data.C_ss(index_opt);
V_opt = sim_data.house_vol(index_opt);
a_inf_opt = sim_data.a_inf(index_opt);
a_nat_opt = sim_data.a_nat(index_opt);
P_opt = sim_data.P(index_opt);

E_pct_ref = prctile(E_ref,0:100);
E_pct_opt = prctile(E_opt,0:100);

C_out_bins = linspace(1,35,101);

x = zeros(1,100);
y = zeros(1,100);

for i = 1:100 % ambient concentration
    for j = 1:100 % emission deciles
        index_cell_ref = E_ref>=E_pct_ref(j)&E_ref<E_pct_ref(j+1)&C_ref>=C_out_bins(i)&C_ref<C_out_bins(i+1);
        index_cell_opt = E_opt>=E_pct_opt(j)&E_opt<E_pct_opt(j+1)&C_opt>=C_out_bins(i)&C_opt<C_out_bins(i+1);
        
        indoor_conc_cell_ref = mean(C_ss_ref(index_cell_ref));
        indoor_conc_cell_opt = mean(C_ss_opt(index_cell_opt));
        
        ratio_cell = indoor_conc_cell_opt/indoor_conc_cell_ref;
        
        grid_panel_c(i,j) = ratio_cell;
        
        indoor_contribution = mean(E_opt(index_cell_opt))/mean(V_opt(index_cell_opt));
        outdoor_contribution = (mean(a_inf_opt(index_cell_opt))*mean(P_opt(index_cell_opt))+mean(a_nat_opt(index_cell_opt)))*mean(C_out_bins(i)+C_out_bins(i+1));
        
        grid_panel_d(i,j) = indoor_contribution/outdoor_contribution;
        
        grid_panel_e(i,j) = indoor_conc_cell_opt/mean(C_out_bins(i)+C_out_bins(i+1));
         
        x(j) = (j+(j+1))/2-1;      
    end
    y(i) = (C_out_bins(i)+C_out_bins(i+1))/2;
end

% Create Z for ref

[X,Y] = meshgrid(x,y) ;

%% Create csv files for igor
csvwrite('PanelCMatrix.csv',grid_panel_c)
csvwrite('PanelDMatrix.csv',grid_panel_d)
csvwrite('PanelEMatrix.csv',grid_panel_e)

%% Create Igor color maps for I/O ratio (panel e)
vector_e = grid_panel_e(:);

PERCENTRANK = @(YourArray, TheProbes) reshape( mean( bsxfun(@le, YourArray(:), TheProbes(:).') ) * 100, size(TheProbes) );

white_pct = PERCENTRANK(vector_d,1);

red_pct = 100;
redwhite_pct = white_pct + 5;
bluewhite_pct = white_pct - 5;
blue_pct = 0;

vec = [red_pct;redwhite_pct;white_pct;bluewhite_pct;blue_pct;];
hex = ['#FF0000';'#FFBDBD';'#FFFFFF';'#BBBBFF';'#0000FF'];
raw = sscanf(hex','#%2x%2x%2x',[3,size(hex,1)]).';
N = 256;
%N = size(get(gcf,'colormap'),1) % size of the current colormap
map = floor(interp1(vec,raw,linspace(100,0,N),'pchip')*257);


csvwrite('PanelEColorMap.csv',map)


%% Create Igor file where you have one long wave
panel_choice = grid_panel_d;
x_c = [];
y_c = [];
z_c = [];

for i = 1:100 % loop over x coordinate, changes by row
    x_i = x(i);
    for j = 1:100 % loop over y coordinate, changes by column
        y_j = y(j);
        z_ij = log10(panel_choice(j,i));
        x_c = [x_c;x_i];
        y_c = [y_c;y_j];
        z_c = [z_c;z_ij];
    end
end
csvwrite('PanelD.csv',[x_c,y_c,z_c])

%%
figure
surf(X,Y,grid_panel_c-1) ;
view([0 90])
xlim([0 100])
ylim([0 35])
set(gca,'FontSize',20)
box on
set(gca,'YMinorTick','off')
set(gca,'TickDir','out');
ylabel('Ambient outdoor PM_{2.5} concentration (?g/m^{3})')
xlabel('Indoor Emission Percentile')
rwb = colormap(bluewhitered());
c = colorbar('Ticks',[-.2 -.1 0 0.1 0.2 0.3],'TickLabels',{'0.8','0.9','1','1.1','1.2','1.3'});
c.Label.String = 'Indoor PM_{2.5} enhancement (C_{Opt}/C_{Ref})';


%%
figure
surf(X,Y,log10(grid_panel_d)) ;
view([0 90])
xlim([0 100])
ylim([0 35])
ylabel('Ambient outdoor PM_{2.5} concentration (ug/m^{3})')
xlabel('Indoor Emission Percentile')
set(gca,'FontSize',20)

box on
set(gca,'YMinorTick','off')
set(gca,'TickDir','out');
rwb = colormap(bluewhitered());
c = colorbar('Ticks',[-1 0 1],'TickLabels',{'0.1','1','10'});
c.Label.String = 'Indoor/Outdoor Ratio';
%set(gca,'ColorScale','log')



%%
figure
surf(X,Y,log10(grid_panel_e));
view([0 90])
xlim([0 100])
ylim([0 35])
ylabel('Ambient Outdoor PM_{2.5} Concentration (ug/m^{3})')
xlabel('Indoor Emission Percentile')

box on
set(gca,'YMinorTick','off')
set(gca,'TickDir','out');
colormap();
rwb = colormap(bluewhitered());
%c = colorbar('Ticks',[-1 0 1 4],'TickLabels',{'0','1','2','5'});
c = colorbar();
c.Label.String = 'Indoor/Outdoor Ratio (Log Scale)';
