# Python notes
_Rongyang Sun_

## Project

### File structure

#### Pure Python project

The minimal file structure for a pure python package project which uses `unittest` as its unit test tool looks like
```python
.
|-- setup.py                     # Distribution stript.
`-- src                          # Source directory.
    |-- pkg1                     # Package `pkg1' directory.
    |   |-- __init__.py          # initial file for `pkg1'.
    |   `-- module1.py           # Source code for module `pkg1.module1'.
    `-- tests                    # Root directory for unit test.
        |-- __init__.py          # Put this file to declare `tests' as a module.
        `-- ut_pkg1              # Unit test directory for unit test of `pkg1' package.
            |-- __init__.py      # Put this file to declare `ut_pkg1' as a module.
            `-- test_module1.py  # Unit test file to test `module1' module.
```
And its typical `setup.py` file looks like:
```python
from setuptools.command.install import install
from setuptools import setup, find_packages


setup(
  name='proj_exps_pure_py',
  version='0.0.1',
  description='A minimal toy pure python project with unit test.',
  author='Rongyang Sun',
  package_dir={'': 'src'},  # Search all packages under `src/' directory.
  packages=find_packages('src', exclude=['tests', 'tests.*']),  # Exclude tests* packages.
  python_requires='>=3.4',  # Python version require.
  test_suite='tests',       # Find unit tests under `src/tests/' directory.
  zip_safe=False,
)
```



## Standard library

A reason why Python is so popular now is that it has a very powerful standard library. The following methods and tricks are only use its standard library.

### System
- Check whether a file exists or not
```python
import os
os.path.isfile(file_path)
```
- Get consistent operating system name. For Linux, print 'Linux', for Mac OSX, print 'Darwin' and for Windows, print 'Windows':

```python
import os
uname = (os.uname()).sysname
import platform
uname = platform.system()
```
## Third party libraries

Another reason why Python is so popular now is that is has a lot of high-quailty and powerful third party libraries (packages) like `numpy`, `scipy` and `matplotlib`.

### Numpy

- Flip an array along the given axis: `np.flip`.

  ```python
  >>> A = np.arange(4)
  >>> A
  array([0, 1, 2, 3])
  >>> np.flip(A, axis=0)
  array([3, 2, 1, 0])
  
  >>> A = np.arange(8).reshape((2,2,2))
  >>> A
  array([[[0, 1],
          [2, 3]],
         [[4, 5],
          [6, 7]]])
  >>> flip(A, 0)
  array([[[4, 5],
          [6, 7]],
         [[0, 1],
          [2, 3]]])
  >>> flip(A, 1)
  array([[[2, 3],
          [0, 1]],
         [[6, 7],
          [4, 5]]])
  >>> np.flip(A)
  array([[[7, 6],
          [5, 4]],
         [[3, 2],
          [1, 0]]])
  >>> np.flip(A, (0, 2))
  array([[[5, 4],
          [7, 6]],
         [[1, 0],
          [3, 2]]])
  
  >>> A = np.random.randn(3,4,5)
  >>> np.all(flip(A,2) == A[:,:,::-1,...])
  True
  ```

- Binding sort n rank 1 tensor (vector) numpy arrays.

  We define `binding_sort` function. This function can use one vector as the benchmark to sort other vectors. The default benchmark vector is the 0th one and the default order is ascending order.

  ```python
  import numpy as np
  def binding_sort(*args, which_one=0, inverse=False):
    sorted_idx = args[which_one].argsort()
    if inverse: sorted_idx = np.flip(sorted_idx, axis=0)
    return list(vec[sorted_idx] for vec in args)
  ```

  Here are some examples

  ```python
  >>> a,b = (np.random.rand(4) for i in [0,1])
  
  >>> a,b
  (array([0.49801333, 0.72046521, 0.0594317 , 0.26922479]), array([0.24147948, 0.78940445, 0.45235445, 0.28484671]))
  
  >>> c,d = binding_sort(a,b), binding_sort(a,b, which_one=1)
  
  >>> c
  [array([0.0594317 , 0.26922479, 0.49801333, 0.72046521]), array([0.45235445, 0.28484671, 0.24147948, 0.78940445])]
  
  >>> d
  [array([0.49801333, 0.26922479, 0.0594317 , 0.72046521]), array([0.24147948, 0.28484671, 0.45235445, 0.78940445])]
  
  >>> e = binding_sort(a, b, inverse=True)
  
  >>> e
  [array([0.72046521, 0.49801333, 0.26922479, 0.0594317 ]), array([0.78940445, 0.24147948, 0.28484671, 0.45235445])]
  ```

  