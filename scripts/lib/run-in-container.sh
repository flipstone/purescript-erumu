if [ "$IN_CONTAINER" ]; then
  # Already in container, nothing to do
  :
else
  # Script was run from outside the container, re-exec inside the container
  # with the same arguments.
  docker compose build dev

  exec docker compose run --rm dev "$@"
fi
