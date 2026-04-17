clear
close all
clc

script_dir = get_script_dir();
output_dir = fullfile(script_dir, 'hw9_partb_output');

if ~exist(output_dir, 'dir')
    mkdir(output_dir);
end

T = 1000;
burnin = 500;
seed = 12345;

par.alpha = 1 / 3;
par.eta = 1;
par.phi = 5;
par.delta = 0.025;
par.xi = 1;
par.beta = 0.99;
par.sigma = 1;
par.rho = 0.9;
par.sigma_eps = 1;
par.sigma_e2 = 0.25;

ss = compute_steady_state(par);
lin = compute_linear_objects(par);
sol = solve_reduced_form(par, ss, lin);
[B, H, Xi] = build_state_space(par, lin, sol);

rng(seed)

TT = T + burnin;
state = zeros(2, TT + 1);
latent_x = zeros(4, TT);
observed_x = zeros(4, TT);
u_draws = zeros(1, TT);
e_draws = zeros(4, TT);

for t = 1:TT
    latent_x(:, t) = H * state(:, t);

    e_t = sqrt(par.sigma_e2) * randn(4, 1);
    observed_x(:, t) = latent_x(:, t) + e_t;
    e_draws(:, t) = e_t;

    w_tp1 = randn;
    u_draws(t) = w_tp1;
    u_tp1 = [0; par.sigma_eps * w_tp1];
    state(:, t + 1) = B * state(:, t) + u_tp1;
end

keep = (burnin + 1):TT;

sim_table = table( ...
    (1:T)', ...
    observed_x(1, keep)', observed_x(2, keep)', observed_x(3, keep)', observed_x(4, keep)', ...
    latent_x(1, keep)', latent_x(2, keep)', latent_x(3, keep)', latent_x(4, keep)', ...
    state(1, keep)', state(2, keep)', ...
    u_draws(keep)', ...
    e_draws(1, keep)', e_draws(2, keep)', e_draws(3, keep)', e_draws(4, keep)', ...
    'VariableNames', { ...
    't', ...
    'c_star', 'i_star', 'y_star', 'l_star', ...
    'c', 'i', 'y', 'l', ...
    'k_lag', 'z', ...
    'w', ...
    'e_c', 'e_i', 'e_y', 'e_l'} ...
);

writetable(sim_table, fullfile(output_dir, 'hw9_partb_simulated_data.csv'));
writematrix(B, fullfile(output_dir, 'hw9_partb_state_B_matrix.csv'));
writematrix(H, fullfile(output_dir, 'hw9_partb_measurement_H_matrix.csv'));
writematrix(Xi, fullfile(output_dir, 'hw9_partb_measurement_Xi_matrix.csv'));

summary_lines = {
    'ECON 5345 HW9 Part (b)'
    '======================'
    ''
    'Policy coefficients [p, q, r, s]:'
    mat2str(sol.x, 10)
    ''
    'State transition matrix B:'
    mat2str(B, 10)
    ''
    'Measurement matrix H:'
    mat2str(H, 10)
    ''
    'Measurement error covariance Xi:'
    mat2str(Xi, 10)
    ''
    'Steady-state objects [R_ss, KY, IY, CY]:'
    mat2str([ss.R_ss, ss.ky, ss.iy, ss.cy], 10)
};

writecell(summary_lines, fullfile(output_dir, 'hw9_partb_summary.txt'));

save(fullfile(output_dir, 'hw9_partb_workspace.mat'), ...
    'par', 'ss', 'lin', 'sol', 'B', 'H', 'Xi', ...
    'state', 'latent_x', 'observed_x', 'u_draws', 'e_draws', ...
    'T', 'burnin', 'seed');


function script_dir = get_script_dir()
script_path = mfilename('fullpath');
if isempty(script_path)
    script_dir = pwd;
else
    script_dir = fileparts(script_path);
end
end


function ss = compute_steady_state(par)
ss.R_ss = 1 / par.beta - (1 - par.delta);
ss.ky = par.alpha / ss.R_ss;
ss.iy = par.delta * ss.ky;
ss.cy = 1 - ss.iy;
ss.Y_ss = 1;
ss.K_ss = ss.ky * ss.Y_ss;
ss.I_ss = ss.iy * ss.Y_ss;
ss.C_ss = ss.cy * ss.Y_ss;
ss.L_ss = ((1 - par.alpha) / (par.xi * ss.C_ss^par.sigma))^(1 / (1 + 1 / par.eta));
end


function lin = compute_linear_objects(par)
lin.A_l = 1 + 1 / par.eta;
den = par.alpha + 1 / par.eta;

lin.gk = par.alpha * lin.A_l / den;
lin.gc = - (1 - par.alpha) * par.sigma / den;
lin.gz = lin.A_l / den;
end


function sol = solve_reduced_form(par, ss, lin)
starts = [ ...
    0.95,  0.05, 0.02, 0.50; ...
    0.90,  0.10, 0.05, 1.00; ...
    0.98,  0.02, 0.01, 0.20; ...
    0.75,  0.15, 0.10, 0.75; ...
    0.60,  0.25, 0.10, 0.10; ...
    0.95,  0.05, 0.20, 0.80; ...
    0.85,  0.15, 0.15, 1.20; ...
    0.99,  0.01, 0.05, 0.10 ...
];

obj = @(x) sum(policy_residuals(x, par, ss, lin).^2);
options = optimset( ...
    'Display', 'off', ...
    'MaxFunEvals', 1e5, ...
    'MaxIter', 1e5, ...
    'TolX', 1e-12, ...
    'TolFun', 1e-12 ...
);

best_x = [];
best_fval = Inf;

for j = 1:size(starts, 1)
    [x_hat, fval] = fminsearch(obj, starts(j, :), options);

    if abs(x_hat(1)) >= 1
        continue
    end

    if fval < best_fval
        best_x = x_hat;
        best_fval = fval;
    end
end

if isempty(best_x)
    error('No stable reduced-form solution found.')
end

sol.x = best_x(:);
sol.residuals = policy_residuals(best_x, par, ss, lin);
sol.objective = best_fval;
end


function res = policy_residuals(x, par, ss, lin)
p = x(1);
q = x(2);
r = x(3);
s = x(4);

res = zeros(4, 1);

res(1) = lin.gk + lin.gc * r - ss.cy * r - ss.ky * (p - (1 - par.delta));
res(2) = lin.gc * s + lin.gz - ss.cy * s - ss.ky * q;
res(3) = par.sigma * r * (p - 1) ...
    - (par.beta * ss.R_ss * p * (lin.gk + lin.gc * r - 1) ...
    + par.beta * par.phi * p * (p - 1) ...
    - par.phi * (p - 1));
res(4) = par.sigma * (r * q + s * (par.rho - 1)) ...
    - (par.beta * ss.R_ss * (q * (lin.gk - 1 + lin.gc * r) + lin.gc * s * par.rho + lin.gz * par.rho) ...
    + par.beta * par.phi * q * (p + par.rho - 1) ...
    - par.phi * q);
end


function [B, H, Xi] = build_state_space(par, lin, sol)
p = sol.x(1);
q = sol.x(2);
r = sol.x(3);
s = sol.x(4);

h_ck = r;
h_cz = s;
h_ik = (p - (1 - par.delta)) / par.delta;
h_iz = q / par.delta;
h_yk = lin.gk + lin.gc * r;
h_yz = lin.gc * s + lin.gz;
h_lk = (-par.sigma * h_ck + h_yk) / lin.A_l;
h_lz = (-par.sigma * h_cz + h_yz) / lin.A_l;

H = [ ...
    h_ck, h_cz; ...
    h_ik, h_iz; ...
    h_yk, h_yz; ...
    h_lk, h_lz ...
];

B = [ ...
    p, q; ...
    0, par.rho ...
];

Xi = par.sigma_e2 * eye(4);
end
