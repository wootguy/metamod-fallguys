

cmake -S "bullet3" -B "bullet3/build" -DCMAKE_CXX_FLAGS=-m32 -DCMAKE_C_FLAGS=-m32 -DUSE_GLUT=FALSE -DUSE_GRAPHICAL_BENCHMARK=FALSE -DBUILD_BULLET2_DEMOS=FALSE -DBUILD_BULLET_EXTRAS=FALSE -DBUILD_CLSOCKET=FALSE -DBUILD_CPU_DEMOS=FALSE -DBUILD_ENET=FALSE -DBUILD_OPENGL3_DEMOS=FALSE -DBUILD_UNIT_TESTS=FALSE -DENABLE_VHACD=FALSE -INSTALL_LIBS=FALSE -INSTALL_EXTRA_LIBS=FALSE -INSTALL_CMAKE_FILES=FALSE -USE_SOFT_BODY_MULTI_BODY_DYNAMICS_WORLD=FALSE

make OPT=opt install