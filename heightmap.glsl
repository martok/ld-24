   glslShaderFiled      1�  '  #version 120

uniform sampler2D uHeightMap; 
uniform vec2 uSize; 

void main(void)
{
    vec4 pos = gl_Vertex;
    pos.y -= 2;
    float height = texture2D(uHeightMap, gl_Position.xz / uSize).r;  
    gl_Position = gl_ModelViewProjectionMatrix * pos; 
    gl_FrontColor = gl_Color;
}0�  C   #version 120

void main(void)
{
    gl_FragColor = gl_Color;
}