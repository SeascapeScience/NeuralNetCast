function [ES]=ECOMONbin(mon,pgon)

load ~/Work/Data/ECOMON/V5/EcoMon_Zooplankton_Data_v2_5.mat

YEARS = min(DATA(:,4)):max(DATA(:,4));

for y=1:length(YEARS)
    I=find(inpolygon(DATA(:,3),DATA(:,2),pgon(:,1),pgon(:,2)) ...
        & DATA(:,4)==YEARS(y) ...
        & DATA(:,5)>=min(mon) & DATA(:,5)<=max(mon));
    es=[YEARS(y) nanmean(DATA(I,10:end))];
    ES(y,1:length(es))=es;
end



