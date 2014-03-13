ENV=LANG=en_US.utf-8
RAKE=bundle exec rake

default:
	$(ENV) $(RAKE) generate

deploy:
	$(ENV) $(RAKE) deploy

preview:
	$(ENV) $(RAKE) preview
