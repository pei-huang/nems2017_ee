function [vol] = HouseVolume(year)
%HOUSEVOLUME Summary of this function goes here
%   Detailed explanation goes here

load('home_vol.mat')
vol = home_volume(year);

end

