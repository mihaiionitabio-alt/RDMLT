from pathlib import Path
import subprocess
root=Path(__file__).resolve().parents[1];b=root/'build';d=root/'dependencies/json-fortran/json-fortran-9.0.5/src'
b.joinpath('scenarios').mkdir(exist_ok=True)
subprocess.run(['gfortran','-ffree-line-length-none',str(root/'tests/scenario_driver.f90'),'-I'+str(b/'gnu-modules'),'-L'+str(b),'-lr2','-Wl,-rpath,'+str(b),'-o',str(b/'scenario_driver')],check=True,cwd=b/'gnu-modules')
subprocess.run([str(b/'scenario_driver')],check=True,cwd=b)
order=['json_kinds','json_parameters','json_string_utilities','json_value_module','json_file_module','json_module']
subprocess.run(['gfortran','-cpp','-O1','-ffree-line-length-none','-I'+str(d),*[str(d/(f+'.F90')) for f in order],str(root/'tests/full_json_validate.f90'),'-o',str(b/'json_validate')],check=True,cwd=b)
