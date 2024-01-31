module.exports = function (grunt) {
  "use strict";

  grunt.initConfig({
    dirs: {
      src: 'src'
    },

    files: {
      src_purs: "<%= dirs.src %>/**/*.purs",
      purescript: [ "<%= files.src_purs %>" ]
    },

    exec: {
      compile_spago: {
        command: 'spago build'
      }
    },

    watch: {
      purescript: {
        files: [ "<%= files.purescript %>" ],
        tasks: [ "default" ]
      }
    }
  });

  grunt.loadNpmTasks("grunt-contrib-watch");
  grunt.loadNpmTasks("grunt-exec");

  grunt.registerTask("default", [ "exec:compile_spago" ]);

  grunt.registerTask("dev", [ "watch:purescript" ]);
}
