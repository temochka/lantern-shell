lock '3.12.1'

set :deploy_to, ENV.fetch('CAPISTRANO_DEPLOY_TO')
set :keep_releases, 5
set :repo_url, 'dist'
set :linked_dirs, %w(.lantern)

namespace :deploy do
  task :build do
    run_locally do
      execute :npm, 'run', 'build:prod'
    end
  end

  task :compress do
    run_locally do
      ext = %w(html js css xml eot svg ttf woff)
      execute :find, fetch(:repo_url), "-type f \\( #{ext.map { |e| "-name \"*.#{e}\"" }.join(' -o ')} \\) -exec sh -c \"gzip -c -9 {} > {}.gz\" \\;"
      execute :cp, '-r', '.schema', fetch(:repo_url)
    end
  end

  task :set_schema_permissions do
    on roles(:web) do
      schema_file = File.join(release_path, '.schema', 'schema.sql')
      execute :touch, schema_file
      execute :chmod, '777', schema_file
    end
  end

  before :starting, :build
  after :build, :compress

  after :updated, :set_schema_permissions
end
