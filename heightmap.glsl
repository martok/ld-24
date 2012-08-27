   glslShaderFiled      1�  �  #version 120

uniform sampler2D uHeightMap; 
uniform vec2 uSize; 
uniform float uHeight;

void main(void)
{
    gl_TexCoord[0] = gl_MultiTexCoord0;
    vec4 pos = gl_Vertex;   
    float height = texture2D(uHeightMap, vec2(gl_TexCoord[0])).r;
    pos.y -= (height-0.5)*uHeight;  
    gl_Position = gl_ModelViewProjectionMatrix * pos;     
    gl_FrontColor = gl_Color;
}0�  G   #version 120

void main(void)
{
    gl_FragColor = gl_Color;    
}