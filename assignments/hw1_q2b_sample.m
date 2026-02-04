clear
close all
clc

% discretization
np  = 15;
nmu = 15;

muMin = 0.8;
muMax = 1.2;

theta = 3;

pMin = theta / (theta-1) * muMin;
pMax = theta / (theta-1) * muMax;

muGrid = linspace(muMin, muMax, nmu)';
pGrid = linspace(pMin, pMax, np)';

kappa = 1;
gmu   = repmat( 1 / nmu, [nmu, 1] );


OPTIONS = optimoptions('fmincon','MaxFunctionEvaluations', 1000000, 'OptimalityTolerance', 1e-20, 'StepTolerance', 1e-20);

f = fmincon(@(f) -profit(f, pGrid, muGrid, theta), repmat( 1/np/nmu, [np, nmu]),...
    [],[],[],[],zeros(np * nmu, 1),[], @(f) const(f, gmu, kappa), OPTIONS); 
% note that we transform the maximization problem into a minimization problem by multiplying -1
% f(i,j) = f(p_i, mu_j). The initial point, repmat( 1/np/nmu, [np, nmu]),
% is a np by nmu matrix. Thus, fmincon automatically consider 'f' as a np 
% by nmu matrix.

f = f / ( muMax - muMin) / ( pMax - pMin );   % transform the probability mass function into a probability density function
% That is, when integrated on [pMin, pMax] * [muMin, muMax], it should be
% 1.

% 3-d plot
[p2, mu2] = ndgrid(pGrid, muGrid);
surf(p2, mu2, f)

function Epi = profit(f, pGrid, muGrid, theta)
[p2, mu2] = ndgrid(pGrid, muGrid);

pi = p2.^(-theta) .* ( p2 - mu2 );  % profit(i,j) = profit given p_i and mu_j.

Epi = pi .* f; % profit(i,j) * f(p_i, mu_j)

Epi = sum(sum(Epi));

end


function [c, ceq] = const(f, gmu, kappa)

fmu = sum(f)';
fp = sum(f, 2);

ceq = fmu - gmu;   % marginal density = prior (uniform)

c = sum( sum( f .* log2( f ./ ( fp * fmu' ) ) )) - kappa;  % mutual information <= kappa 

end

