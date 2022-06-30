MODNAME = fallguys
SRCFILES = dllapi.cpp engine_api.cpp engine_hook.cpp fallguys.cpp h_export.cpp meta_api.cpp physics.cpp server_hook.cpp
INCLUDEDIRS+=-I$(SDKSRC)/../bullet3/src
EXTRA_CFLAGS+=-DPLATFORM_POSIX
EXTRA_LINK+=-L$(SDKSRC)/../bullet3/build/src/Bullet3Dynamics
EXTRA_LINK+=-L$(SDKSRC)/../bullet3/build/src/Bullet3Collision
EXTRA_LINK+=-L$(SDKSRC)/../bullet3/build/src/Bullet3Common
EXTRA_LINK+=-L$(SDKSRC)/../bullet3/build/src/Bullet3Geometry
EXTRA_LINK+=-L$(SDKSRC)/../bullet3/build/src/BulletSoftBody
EXTRA_LINK+=-L$(SDKSRC)/../bullet3/build/src/BulletDynamics
EXTRA_LINK+=-L$(SDKSRC)/../bullet3/build/src/BulletCollision
EXTRA_LINK+=-L$(SDKSRC)/../bullet3/build/src/LinearMath
EXTRA_LINK+=-Wl,--whole-archive -lBullet3Dynamics -lBullet3Collision -lBullet3Common -lBullet3Geometry -lBulletSoftBody -lBulletDynamics -lBulletCollision -lLinearMath -Wl,--no-whole-archive