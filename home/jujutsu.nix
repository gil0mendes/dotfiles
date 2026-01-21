{
  config,
  ...
}:
let
  isWork = config.home.user-info.username == "gmendes";
in
{
  programs.jujutsu = {
    enable = true;
    settings = {
      user = {
        name = config.home.user-info.fullName;
        email = config.home.user-info.email;
      };
      ui = {
        color = "auto";
        editor = "nvim";
        paginate = "auto";

        # Make prev/next default to --edit mode (edit commit in place)
        # instead of creating a working copy children
        movement = {
        	edit = true;
        };
      };
      signing = {
        behavior = "own";
        backend = "gpg";
      }
      // (
        if isWork then
          {
            key = "3900180E4467EA40BB5CC04411EC8E1407151F84";
          }
        else
          {
            key = "3C0FFA89D9EB7EF4BDABAB1B8108024DFE52031C";
          }
      );
      git = {
        signOnPush = true;
      };
      fsmonitor = {
      	# Use Watchman for faster working copy snapshotting
        backend = "watchman";
      };
      template-aliases = {
	      # Show relative timestamps like "2 hours ago" instead of full dates
	      "format_timestamp(timestamp)" = "timestamp.ago()";
      };
      revset-aliases = {
        # Show commits authored by me
        "mine()" = "author(gil0mendes)";

        # Your commits that aren't immutable (trunk/tags/remote branches)
        # Shows all your mutable work across the repo
        "wip()" = "mine() ~ immutable()";

        # Your commits not yet merged into trunk
        # Shows your unmerged/unpushed work
        "open()" = "mine() ~ ::trunk()";

        # Show current branch from @ to its leaf commits
        "current()" = "@:: & mutable()";

        # Show your entire stack - all mutable commits connected to @
        # reachable() finds commits reachable in any direction (ancestors + descendants)
        # within the mutable() set (excludes immutable trunk/tags)
        "stack()" = "reachable(@, mutable())";
      };
      aliases = {
	     	bm = ["bookmark"];
	      gf = ["git" "fetch"];
	      gp = ["git" "push"];

				# Show your entire connected stack
				stack = ["log" "-r" "stack()"];

				# Advance a bookmark to your latest pushable change
				# Usage: jj tug [bookmark_name]
				# - jj tug        → moves closest_bookmark(@) to closest_pushable(@)
				# - jj tug main   → moves 'main' to closest_pushable(@)
				tug = ["util" "exec" "--" "sh" "-c" """
				  if [ -n \"$1\" ]; then
				    jj bookmark move \"$1\" --to 'closest_pushable(@)'
				  else
				    jj bookmark move --from 'closest_bookmark(@)' --to 'closest_pushable(@)'
				  fi
				""" "sh"];
      };
    };
  };
}
