lock '3.12.1'

set :deploy_to, ENV.fetch('CAPISTRANO_DEPLOY_TO')
set :keep_releases, 5
set :repo_url, 'dist'
set :tarball_exclude, %w(.lantern)
set :linked_dirs, %w(.lantern)

namespace :deploy do
  task :build do
    run_locally do
      execute :npm, 'run', 'clean'
      execute :npm, 'run', 'build:prod'
    end
  end

  task :compress do
    run_locally do
      ext = %w(html js css xml eot svg ttf woff)
      execute :find, fetch(:repo_url), "-type f \\( #{ext.map { |e| "-name \"*.#{e}\"" }.join(' -o ')} \\) -exec sh -c \"gzip -c -9 {} > {}.gz\" \\;"
    end
  end

  task :set_schema_permissions do
    on roles(:web) do
      schema_dir = File.join(release_path, '.schema')
      execute :find, schema_dir, '-type', 'd', '-exec', 'chmod', '777', '{}', '+'
      execute :find, schema_dir, '-type', 'f', '-exec', 'chmod', '666', '{}', '+'
    end
  end

  before :starting, :build
  after :build, :compress
  after :updated, :set_schema_permissions
end
